use crate::ast::Type;
use crate::{ast, lexer, parser};
use anyhow::{Context, Result, anyhow};
use codespan::Files;
use std::collections::{HashMap, HashSet, VecDeque};
use std::env;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::fs;
use sha2::{Sha256, Digest};

#[derive(Debug, Clone)]
pub struct ModuleMetadata {
    pub path: PathBuf,
    pub module_type: ast::ModuleType,
    pub exported_functions: Vec<String>,
    pub exported_structs: Vec<String>,
    pub exported_variables: Vec<String>,
    pub dependencies: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct ImportResolver {
    pub resolved_modules: HashMap<String, ModuleMetadata>,
}

impl ImportResolver {
    pub fn new() -> Self {
        Self {
            resolved_modules: HashMap::new(),
        }
    }

    pub fn resolve_import_path(
        &self,
        module_path: &str,
        module_type: &ast::ModuleType,
        base_path: &Path,
    ) -> Result<PathBuf> {
        match module_type {
            ast::ModuleType::Standard => resolve_standard_library_path(module_path),
            ast::ModuleType::Local => {
                let current_dir = base_path
                    .parent()
                    .ok_or_else(|| anyhow!("Base path has no parent"))?;
                Ok(current_dir.join(module_path))
            }
            ast::ModuleType::External => resolve_library_path(module_path),
        }
    }

    pub fn collect_module_exports(&self, program: &ast::Program) -> ModuleMetadata {
        let mut exported_functions = Vec::new();
        let mut exported_structs = Vec::new();
        let mut exported_variables = Vec::new();

        for function in &program.functions {
            if matches!(function.visibility, ast::Visibility::Public | ast::Visibility::Internal) {
                exported_functions.push(function.name.clone());
            }
        }

        for struct_def in &program.structs {
            if matches!(struct_def.visibility, ast::Visibility::Public) {
                exported_structs.push(struct_def.name.clone());
            }
        }

        for stmt in &program.stmts {
            if let ast::Stmt::Let(name, _, _, _, visibility) = stmt {
                if matches!(visibility, ast::Visibility::Public | ast::Visibility::Internal) {
                    exported_variables.push(name.clone());
                }
            }
        }

        ModuleMetadata {
            path: PathBuf::new(),
            module_type: ast::ModuleType::Local,
            exported_functions,
            exported_structs,
            exported_variables,
            dependencies: Vec::new(),
        }
    }


    pub fn resolve_module_dependencies(
        &mut self,
        module_path: &str,
        module_type: &ast::ModuleType,
        base_path: &Path,
    ) -> Result<Vec<PathBuf>> {
        let resolved_path = self.resolve_import_path(module_path, module_type, base_path)?
            .canonicalize()
            .with_context(|| format!("Failed to canonicalize path for: {}", module_path))?;

        let mut dependencies = Vec::new();
        dependencies.push(resolved_path.clone());

        if let Some(metadata) = self.resolved_modules.get(module_path) {
            dependencies.extend(metadata.dependencies.clone());
        }

        Ok(dependencies)
    }

    pub fn cache_module_metadata(&mut self, module_path: String, metadata: ModuleMetadata) {
        self.resolved_modules.insert(module_path, metadata);
    }
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub file_path: PathBuf,
    pub content_hash: String,
    pub last_modified: SystemTime,
    pub imports: Vec<ast::ImportDeclaration>,
    pub program: Option<ast::Program>,
    pub is_dirty: bool,
}

impl ModuleInfo {
    pub fn new(file_path: PathBuf) -> Result<Self> {
        let metadata = fs::metadata(&file_path)?;
        let last_modified = metadata.modified()?;
        let content = fs::read_to_string(&file_path)?;
        let content_hash = Self::calculate_hash(&content);
        
        Ok(ModuleInfo {
            file_path,
            content_hash,
            last_modified,
            imports: Vec::new(),
            program: None,
            is_dirty: true,
        })
    }
    
    pub fn calculate_hash(content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }
    
    pub fn update_if_changed(&mut self) -> Result<bool> {
        let metadata = fs::metadata(&self.file_path)?;
        let current_modified = metadata.modified()?;
        
        if current_modified > self.last_modified {
            let content = fs::read_to_string(&self.file_path)?;
            let new_hash = Self::calculate_hash(&content);
            
            if new_hash != self.content_hash {
                self.content_hash = new_hash;
                self.last_modified = current_modified;
                self.is_dirty = true;
                self.program = None;
                return Ok(true);
            }
        }
        Ok(false)
    }
}

#[derive(Debug)]
pub struct ModuleGraph {
    pub modules: HashMap<PathBuf, ModuleInfo>,
    pub dependency_edges: HashMap<PathBuf, Vec<PathBuf>>,
    pub reverse_edges: HashMap<PathBuf, Vec<PathBuf>>,
}

impl ModuleGraph {
    pub fn new() -> Self {
        ModuleGraph {
            modules: HashMap::new(),
            dependency_edges: HashMap::new(),
            reverse_edges: HashMap::new(),
        }
    }
    
    pub fn add_module(&mut self, module_info: ModuleInfo) {
        let path = module_info.file_path.clone();
        self.modules.insert(path.clone(), module_info);
        self.dependency_edges.entry(path.clone()).or_insert_with(Vec::new);
        self.reverse_edges.entry(path).or_insert_with(Vec::new);
    }
    
    pub fn add_dependency(&mut self, from: &Path, to: &Path) -> Result<()> {
        if self.would_create_cycle(from, to)? {
            return Err(anyhow!("Adding dependency from {} to {} would create a cycle", 
                              from.display(), to.display()));
        }
        
        self.dependency_edges.entry(from.to_path_buf())
            .or_insert_with(Vec::new)
            .push(to.to_path_buf());
            
        self.reverse_edges.entry(to.to_path_buf())
            .or_insert_with(Vec::new)
            .push(from.to_path_buf());
            
        Ok(())
    }
    
    fn would_create_cycle(&self, from: &Path, to: &Path) -> Result<bool> {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(to.to_path_buf());
        
        while let Some(current) = queue.pop_front() {
            if current == from {
                return Ok(true);
            }
            
            if visited.insert(current.clone()) {
                if let Some(deps) = self.dependency_edges.get(&current) {
                    for dep in deps {
                        queue.push_back(dep.clone());
                    }
                }
            }
        }
        
        Ok(false)
    }
    
    pub fn topological_sort(&self) -> Result<Vec<PathBuf>> {
        let mut in_degree: HashMap<PathBuf, usize> = HashMap::new();
        let mut result = Vec::new();
        let mut queue = VecDeque::new();
        
        for module_path in self.modules.keys() {
            let degree = self.reverse_edges.get(module_path)
                .map(|deps| deps.len())
                .unwrap_or(0);
            in_degree.insert(module_path.clone(), degree);
            
            if degree == 0 {
                queue.push_back(module_path.clone());
            }
        }
        
        while let Some(current) = queue.pop_front() {
            result.push(current.clone());
            
            if let Some(dependencies) = self.dependency_edges.get(&current) {
                for dep in dependencies {
                    if let Some(degree) = in_degree.get_mut(dep) {
                        *degree -= 1;
                        if *degree == 0 {
                            queue.push_back(dep.clone());
                        }
                    }
                }
            }
        }
        
        if result.len() != self.modules.len() {
            return Err(anyhow!("Cycle detected in module dependencies"));
        }
        
        Ok(result)
    }
    
    pub fn get_dependents(&self, module_path: &Path) -> Vec<PathBuf> {
        let mut dependents = HashSet::new();
        let mut queue = VecDeque::new();
        
        if let Some(direct_dependents) = self.reverse_edges.get(module_path) {
            for dependent in direct_dependents {
                queue.push_back(dependent.clone());
            }
        }
        
        while let Some(current) = queue.pop_front() {
            if dependents.insert(current.clone()) {
                if let Some(transitive_dependents) = self.reverse_edges.get(&current) {
                    for dependent in transitive_dependents {
                        queue.push_back(dependent.clone());
                    }
                }
            }
        }
        
        dependents.into_iter().collect()
    }
    
    pub fn mark_dirty_cascade(&mut self, changed_module: &Path) {
        let dependents = self.get_dependents(changed_module);
        
        if let Some(module) = self.modules.get_mut(changed_module) {
            module.is_dirty = true;
        }
        
        for dependent in dependents {
            if let Some(module) = self.modules.get_mut(&dependent) {
                module.is_dirty = true;
            }
        }
    }
}

pub struct ContentHasher;

impl ContentHasher {
    pub fn hash_file(file_path: &Path) -> Result<String> {
        let content = fs::read_to_string(file_path)
            .with_context(|| format!("Failed to read file: {}", file_path.display()))?;
        Ok(Self::hash_content(&content))
    }
    
    pub fn hash_content(content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

#[derive(Debug)]
pub struct HashCache {
    cache: HashMap<PathBuf, (String, SystemTime)>,
}

impl HashCache {
    pub fn new() -> Self {
        HashCache {
            cache: HashMap::new(),
        }
    }
    
    pub fn get_or_compute(&mut self, file_path: &Path) -> Result<String> {
        let metadata = fs::metadata(file_path)?;
        let modified_time = metadata.modified()?;
        
        if let Some((cached_hash, cached_time)) = self.cache.get(file_path) {
            if *cached_time >= modified_time {
                return Ok(cached_hash.clone());
            }
        }
        
        let hash = ContentHasher::hash_file(file_path)?;
        self.cache.insert(file_path.to_path_buf(), (hash.clone(), modified_time));
        
        Ok(hash)
    }
    
    pub fn invalidate(&mut self, file_path: &Path) {
        self.cache.remove(file_path);
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CacheEntry {
    pub module_path: PathBuf,
    pub content_hash: String,
    pub last_compiled: SystemTime,
    pub dependencies: Vec<PathBuf>,
    pub compilation_artifacts: Vec<PathBuf>,
}

impl CacheEntry {
    pub fn new(
        module_path: PathBuf,
        content_hash: String,
        dependencies: Vec<PathBuf>,
    ) -> Self {
        CacheEntry {
            module_path,
            content_hash,
            last_compiled: SystemTime::now(),
            dependencies,
            compilation_artifacts: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct BuildCacheManager {
    cache_dir: PathBuf,
    entries: HashMap<PathBuf, CacheEntry>,
    hash_cache: HashCache,
    config: CacheConfig,
}

impl BuildCacheManager {
    pub fn new(build_dir: &Path) -> Self {
        let cache_dir = build_dir.join(".cache");
        BuildCacheManager {
            cache_dir,
            entries: HashMap::new(),
            hash_cache: HashCache::new(),
            config: CacheConfig::default(),
        }
    }
    
    pub fn initialize(&mut self) -> Result<()> {
        self.ensure_cache_structure()?;
        self.load_cache_index()?;
        self.validate_cache_entries()?;
        Ok(())
    }
    
    fn ensure_cache_structure(&self) -> Result<()> {
        let dirs = [
            &self.cache_dir,
            &self.cache_dir.join("modules"),
            &self.cache_dir.join("artifacts"),
            &self.cache_dir.join("temp"),
        ];
        
        for dir in dirs {
            if !dir.exists() {
                fs::create_dir_all(dir)
                    .with_context(|| format!("Failed to create cache directory: {}", dir.display()))?;
            }
        }
        
        Ok(())
    }
    
    fn get_cache_index_path(&self) -> PathBuf {
        self.cache_dir.join("cache_index.json")
    }
    
    fn load_cache_index(&mut self) -> Result<()> {
        let index_path = self.get_cache_index_path();
        
        if !index_path.exists() {
            return Ok(());
        }
        
        let content = fs::read_to_string(&index_path)
            .with_context(|| format!("Failed to read cache index: {}", index_path.display()))?;
            
        self.entries = serde_json::from_str(&content)
            .with_context(|| "Failed to parse cache index JSON")?;
            
        Ok(())
    }
    
    fn save_cache_index(&self) -> Result<()> {
        let index_path = self.get_cache_index_path();
        
        let content = serde_json::to_string_pretty(&self.entries)
            .with_context(|| "Failed to serialize cache index")?;
            
        fs::write(&index_path, content)
            .with_context(|| format!("Failed to write cache index: {}", index_path.display()))?;
            
        Ok(())
    }
    
    fn validate_cache_entries(&mut self) -> Result<()> {
        let mut invalid_entries = Vec::new();
        
        for (module_path, entry) in &self.entries {
            if !module_path.exists() {
                invalid_entries.push(module_path.clone());
                continue;
            }
            
            match self.hash_cache.get_or_compute(module_path) {
                Ok(current_hash) => {
                    if current_hash != entry.content_hash {
                        invalid_entries.push(module_path.clone());
                    }
                }
                Err(_) => {
                    invalid_entries.push(module_path.clone());
                }
            }
        }
        
        for invalid_path in invalid_entries {
            self.invalidate_entry(&invalid_path)?;
        }
        
        if !self.entries.is_empty() {
            self.save_cache_index()?;
        }
        
        Ok(())
    }
    
    pub fn is_module_cached(&mut self, module_path: &Path) -> Result<bool> {
        if !Self::should_cache_module(module_path, &self.config) {
            return Ok(false);
        }
        
        if let Some(entry) = self.entries.get(module_path) {
            let current_hash = self.hash_cache.get_or_compute(module_path)?;
            Ok(current_hash == entry.content_hash)
        } else {
            Ok(false)
        }
    }
    
    pub fn cache_module(
        &mut self,
        module_path: &Path,
        dependencies: Vec<PathBuf>,
        artifacts: Vec<PathBuf>,
    ) -> Result<()> {
        if Self::should_cache_module(module_path, &self.config) {
            let content_hash = self.hash_cache.get_or_compute(module_path)?;
            
            let module_cache_name = Self::path_to_cache_name(module_path);
            let module_cache_dir = self.cache_dir.join("modules").join(&module_cache_name);
            let artifacts_dir = self.cache_dir.join("artifacts").join(&module_cache_name);
            
            if module_cache_dir.exists() {
                fs::remove_dir_all(&module_cache_dir)?;
            }
            if artifacts_dir.exists() {
                fs::remove_dir_all(&artifacts_dir)?;
            }
            
            fs::create_dir_all(&module_cache_dir)?;
            fs::create_dir_all(&artifacts_dir)?;
            
            let mut cached_artifacts = Vec::new();
            
            for artifact_path in &artifacts {
                if artifact_path.exists() {
                    let file_name = artifact_path.file_name()
                        .ok_or_else(|| anyhow!("Invalid artifact path: {}", artifact_path.display()))?;
                    let cached_artifact_path = artifacts_dir.join(file_name);
                    
                    fs::copy(artifact_path, &cached_artifact_path)
                        .with_context(|| format!("Failed to copy artifact {} to cache", artifact_path.display()))?;
                    
                    cached_artifacts.push(cached_artifact_path);
                }
            }
            
            let mut entry = CacheEntry::new(
                module_path.to_path_buf(),
                content_hash,
                dependencies,
            );
            entry.compilation_artifacts = cached_artifacts;
            
            self.entries.insert(module_path.to_path_buf(), entry);
            self.save_cache_index()?;
        }
        
        Ok(())
    }

    fn should_cache_module(module_path: &Path, config: &CacheConfig) -> bool {
        if let Some(lib_path) = Self::find_lib_directory(module_path) {
            if module_path.starts_with(&lib_path) {
                let relative_path = module_path.strip_prefix(&lib_path).unwrap_or(module_path);
                let path_str = relative_path.to_string_lossy();
                
                if path_str.starts_with("std/") || path_str.starts_with("std\\") {
                    return config.cache_std_modules;
                }
                
                if config.cache_lib_modules {
                    for excluded in &config.excluded_dirs {
                        if path_str.contains(excluded) {
                            return false;
                        }
                    }
                    return true;
                }
            }
        }
        
        false
    }
    
    fn find_lib_directory(from_path: &Path) -> Option<PathBuf> {
        let mut current = from_path;
        
        while let Some(parent) = current.parent() {
            let lib_candidate = parent.join("lib");
            if lib_candidate.exists() && lib_candidate.is_dir() {
                return Some(lib_candidate);
            }
            current = parent;
        }
        
        let standard_locations = [
            PathBuf::from("lib"),
            std::env::current_dir().unwrap_or_default().join("lib"),
        ];
        
        for location in &standard_locations {
            if location.exists() && location.is_dir() {
                return Some(location.clone());
            }
        }
        
        None
    }
    
    pub fn invalidate_entry(&mut self, module_path: &Path) -> Result<()> {
        if let Some(entry) = self.entries.remove(module_path) {
            for artifact in &entry.compilation_artifacts {
                if artifact.exists() {
                    let _ = fs::remove_file(artifact);
                }
            }
        }
        
        self.hash_cache.invalidate(module_path);
        
        let module_cache_dir = self.cache_dir.join("modules")
            .join(Self::path_to_cache_name(module_path));
        if module_cache_dir.exists() {
            fs::remove_dir_all(module_cache_dir)?;
        }
        
        self.save_cache_index()?;
        Ok(())
    }
    
    pub fn invalidate_dependents(&mut self, changed_module: &Path) -> Result<Vec<PathBuf>> {
        let mut invalidated = Vec::new();
        let mut to_invalidate = Vec::new();
        
        for (module_path, entry) in &self.entries {
            if entry.dependencies.contains(&changed_module.to_path_buf()) {
                to_invalidate.push(module_path.clone());
            }
        }
        
        for module_path in to_invalidate {
            self.invalidate_entry(&module_path)?;
            invalidated.push(module_path);
        }
        
        Ok(invalidated)
    }
    
    fn path_to_cache_name(path: &Path) -> String {
        let mut hasher = Sha256::new();
        hasher.update(path.to_string_lossy().as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

#[cfg(target_os = "windows")]
pub fn prepare_windows_clang_args(
    output: &Path,
    optimize: bool,
    c_file: &Path,
) -> Result<Vec<String>> {
    let msvc_lib_paths = get_msvc_lib_paths()?;
    let mut clang_args = vec![
        if optimize { "-O3" } else { "-O0" }.to_string(),
        "-pipe".to_string(),
        "-fno-exceptions".to_string(),
        c_file.to_str().unwrap().into(),
        "-o".to_string(),
        output.to_str().unwrap().into(),
    ];

    for path in msvc_lib_paths {
        clang_args.push("-L".to_string());
        clang_args.push(path);
    }

    clang_args.extend_from_slice(&[
        "-lmsvcrt".to_string(),
        "-Xlinker".to_string(),
        "/NODEFAULTLIB:libcmt".to_string(),
    ]);

    Ok(clang_args)
}

pub fn resolve_imports_only(
    imports: &[ast::ImportDeclaration],
    base_path: &Path,
) -> Result<Vec<ResolvedImport>> {
    let resolver = ImportResolver::new();
    let mut resolved = Vec::new();
    
    for import in imports {
        match import {
            ast::ImportDeclaration::ImportAll { module_path, module_type, alias } => {
                let resolved_path = resolver.resolve_import_path(module_path, module_type, base_path)?
                    .canonicalize()
                    .with_context(|| format!("Failed to canonicalize path for: {}", module_path))?;
                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::All { alias: alias.clone() },
                    module_path: module_path.clone(),
                });
            }
            ast::ImportDeclaration::ImportSpecifiers { module_path, module_type, specifiers } => {
                let resolved_path = resolver.resolve_import_path(module_path, module_type, base_path)?
                    .canonicalize()
                    .with_context(|| format!("Failed to canonicalize path for: {}", module_path))?;
                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::Specifiers { specifiers: specifiers.clone() },
                    module_path: module_path.clone(),
                });
            }
        }
    }
    Ok(resolved)
}



fn resolve_standard_library_path(module_path: &str) -> Result<PathBuf> {
    let lib_dir = get_lib_path()?;

    if module_path.starts_with("std/") {
        let path_without_prefix = &module_path[4..];
        let mut path = lib_dir.join("std").join("src");

        if !path_without_prefix.is_empty() {
            path = path.join(path_without_prefix);
        }

        let ve_file = path.with_extension("ve");
        if ve_file.exists() {
            return Ok(ve_file);
        }

        let index_file = path.join("index.ve");
        if index_file.exists() {
            return Ok(index_file);
        }
    }

    Err(anyhow!(
        "Standard library module '{}' not found",
        module_path
    ))
}

fn resolve_library_path(module_path: &str) -> Result<PathBuf> {
    let lib_dir = get_lib_path()?;
    let components: Vec<&str> = module_path.split('/').collect();
    if components.is_empty() {
        return Err(anyhow!("Invalid module path: {}", module_path));
    }

    let mut path = lib_dir.join(components[0]).join("src");
    for component in &components[1..] {
        path.push(component);
    }

    let ve_file = path.with_extension("ve");
    if ve_file.exists() {
        Ok(ve_file)
    } else {
        let index_file = path.join("index.ve");
        if index_file.exists() {
            Ok(index_file)
        } else {
            Err(anyhow!("Module '{}' not found in library", module_path))
        }
    }
}

#[cfg(target_os = "windows")]
fn get_msvc_lib_paths() -> Result<Vec<String>> {
    use std::env;
    use std::fs;
    
    let mut paths = Vec::new();

    if let Ok(vc_dir) = env::var("VCINSTALLDIR") {
        let lib_path = format!("{}\\Lib\\x64", vc_dir.trim_end_matches('\\'));
        if Path::new(&lib_path).exists() {
            paths.push(lib_path);
        }
    }

    if let Ok(windows_sdk_dir) = env::var("WindowsSdkDir") {
        let version = env::var("WindowsSDKVersion").unwrap_or_else(|_| {
            let lib_dir = format!("{}\\Lib", windows_sdk_dir.trim_end_matches('\\'));
            if let Ok(entries) = fs::read_dir(&lib_dir) {
                let mut versions: Vec<String> = entries
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
                    .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
                    .filter(|name| name.starts_with("10.0."))
                    .collect();
                versions.sort();
                versions.last().unwrap_or(&"10.0.22621.0".to_string()).clone()
            } else {
                "10.0.22621.0".to_string()
            }
        });
        
        let um_path = format!(
            "{}\\Lib\\{}\\um\\x64",
            windows_sdk_dir.trim_end_matches('\\'),
            version.trim_end_matches('\\')
        );
        let ucrt_path = format!(
            "{}\\Lib\\{}\\ucrt\\x64",
            windows_sdk_dir.trim_end_matches('\\'),
            version.trim_end_matches('\\')
        );
        
        if Path::new(&um_path).exists() {
            paths.push(um_path);
        }
        if Path::new(&ucrt_path).exists() {
            paths.push(ucrt_path);
        }
    }

    if paths.is_empty() {
        let possible_vs_paths = vec![
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise",
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Professional", 
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community",
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\BuildTools",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Enterprise",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Professional",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools",
        ];

        for vs_path in possible_vs_paths {
            let vc_tools_path = format!("{}\\VC\\Tools\\MSVC", vs_path);
            if let Ok(entries) = fs::read_dir(&vc_tools_path) {
                let mut versions: Vec<String> = entries
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
                    .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
                    .collect();
                versions.sort();
                
                if let Some(latest_version) = versions.last() {
                    let lib_path = format!("{}\\VC\\Tools\\MSVC\\{}\\lib\\x64", vs_path, latest_version);
                    if Path::new(&lib_path).exists() {
                        paths.push(lib_path);
                        break;
                    }
                }
            }
        }

        let sdk_paths = vec![
            "C:\\Program Files (x86)\\Windows Kits\\10\\Lib",
            "C:\\Program Files\\Windows Kits\\10\\Lib",
        ];

        for sdk_path in sdk_paths {
            if let Ok(entries) = fs::read_dir(sdk_path) {
                let mut versions: Vec<String> = entries
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
                    .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
                    .filter(|name| name.starts_with("10.0."))
                    .collect();
                versions.sort();
                
                if let Some(latest_version) = versions.last() {
                    let um_path = format!("{}\\{}\\um\\x64", sdk_path, latest_version);
                    let ucrt_path = format!("{}\\{}\\ucrt\\x64", sdk_path, latest_version);
                    
                    if Path::new(&um_path).exists() {
                        paths.push(um_path);
                    }
                    if Path::new(&ucrt_path).exists() {
                        paths.push(ucrt_path);
                    }
                    break;
                }
            }
        }
    }

    if paths.is_empty() {
        return Err(anyhow!(
            "Could not find MSVC libraries. Please ensure Visual Studio Build Tools are installed.\n\
            Install from: https://aka.ms/vs/17/release/vs_BuildTools.exe\n\
            Or run: vcvars64.bat to set up the environment."
        ));
    }

    for path in &paths {
        if !Path::new(path).exists() {
            eprintln!("Warning: Library path does not exist: {}", path);
        }
    }

    let existing_paths: Vec<String> = paths
        .into_iter()
        .filter(|path| Path::new(path).exists())
        .collect();

    if existing_paths.is_empty() {
        return Err(anyhow!(
            "No valid MSVC library paths found. Please install Visual Studio Build Tools.\n\
            Install from: https://aka.ms/vs/17/release/vs_BuildTools.exe"
        ));
    }

    Ok(existing_paths)
}

pub fn validate_ve_file(path: &str) -> std::result::Result<PathBuf, String> {
    let path = Path::new(path);
    let path = if path.extension().is_none() {
        path.with_extension("ve")
    } else {
        path.to_path_buf()
    };

    if !path.exists() {
        let suggestions = suggest_similar_files(&*path.clone())
            .map(|s| format!("\nDid you mean:\n{}", s))
            .unwrap_or_default();

        return Err(format!(
            "File '{}' not found.{}",
            path.display(),
            suggestions
        ));
    }
    Ok(path)
}

fn suggest_similar_files(missing_path: &Path) -> Option<String> {
    let dir = missing_path.parent()?;
    let target_name = missing_path.file_stem()?.to_string_lossy();
    let target_name = target_name.as_ref();

    let matches: Vec<_> = dir
        .read_dir()
        .ok()?
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            let name = path.file_stem()?.to_string_lossy();
            (name.contains(target_name) && path.extension() == Some("ve".as_ref()))
                .then_some(format!("  • {}", path.display()))
        })
        .collect();

    (!matches.is_empty()).then(|| matches.join("\n"))
}

fn get_lib_path() -> Result<PathBuf> {
    if let Ok(exe_path) = env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            let lib_dir = exe_dir.join("lib");
            if lib_dir.exists() {
                return Ok(lib_dir);
            }
        }

        let project_root = exe_path
            .parent()
            .and_then(|p| p.parent())
            .and_then(|p| p.parent());

        if let Some(root) = project_root {
            let lib_dir = root.join("lib");
            if lib_dir.exists() {
                return Ok(lib_dir);
            }
        }

        let deeper_root = exe_path
            .parent()
            .and_then(|p| p.parent())
            .and_then(|p| p.parent())
            .and_then(|p| p.parent());

        if let Some(root) = deeper_root {
            let lib_dir = root.join("lib");
            if lib_dir.exists() {
                return Ok(lib_dir);
            }
        }
    }

    let mut potential_paths = Vec::new();

    if let Ok(home) = env::var("HOME") {
        potential_paths.push(PathBuf::from(home).join(".velang").join("lib"));
    }

    if let Ok(userprofile) = env::var("USERPROFILE") {
        potential_paths.push(PathBuf::from(userprofile).join(".velang").join("lib"));
    }

    potential_paths.extend(vec![
        PathBuf::from("/usr/local/share/velang/lib"),
        PathBuf::from("/opt/velang/lib"),
        PathBuf::from("/usr/share/velang/lib"),
    ]);

    for path in &potential_paths {
        if path.exists() {
            return Ok(path.clone());
        }
    }

    let cwd_lib = env::current_dir().unwrap_or_default().join("lib");
    if cwd_lib.exists() {
        return Ok(cwd_lib);
    }

    let attempted_paths: Vec<String> = potential_paths
        .iter()
        .map(|p| format!("  - {}", p.display()))
        .collect();

    Err(anyhow::anyhow!(
        "Library directory not found. VeLang requires the standard library to be installed.\n\
        Tried looking in:\n\
        - Directory relative to executable\n\
        {}\n\
        - ./lib (current directory)\n\
        \n\
        Please reinstall VeLang or ensure the standard library is properly installed.\n\
        You can install VeLang using the installation script from:\n\
        https://github.com/veil-lang/veil",
        attempted_paths.join("\n")
    ))
}






fn collect_variable_dependencies_from_block(
    stmts: &[ast::Stmt],
    dependencies: &mut HashSet<String>,
) {
    for stmt in stmts {
        collect_variable_dependencies(stmt, dependencies);
    }
}

fn collect_variable_dependencies(stmt: &ast::Stmt, dependencies: &mut HashSet<String>) {
    match stmt {
        ast::Stmt::Let(_, _, expr, _, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Stmt::Return(expr, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Stmt::Block(stmts, _) => {
            for stmt in stmts {
                collect_variable_dependencies(stmt, dependencies);
            }
        }
        ast::Stmt::If(condition, then_stmt, else_stmt, _) => {
            collect_expr_dependencies(condition, dependencies);
            collect_variable_dependencies_from_block(then_stmt, dependencies);
            if let Some(else_stmt) = else_stmt {
                collect_variable_dependencies_from_block(else_stmt, dependencies);
            }
        }
        ast::Stmt::While(condition, body, _) => {
            collect_expr_dependencies(condition, dependencies);
            collect_variable_dependencies_from_block(body, dependencies);
        }
        ast::Stmt::For(_, _, iter_expr, _, body, _) => {
            collect_expr_dependencies(iter_expr, dependencies);
            collect_variable_dependencies_from_block(body, dependencies);
        }
        ast::Stmt::Expr(expr, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        _ => {}
    }
}

fn collect_expr_dependencies(expr: &ast::Expr, dependencies: &mut HashSet<String>) {
    match expr {
        ast::Expr::Var(name, _) => {
            dependencies.insert(name.clone());
        }
        ast::Expr::BinOp(left, _, right, _) => {
            collect_expr_dependencies(left, dependencies);
            collect_expr_dependencies(right, dependencies);
        }
        ast::Expr::UnaryOp(_, expr, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Expr::Call(_, args, _) => {
            for arg in args {
                collect_expr_dependencies(arg, dependencies);
            }
        }
        ast::Expr::FfiCall(_, args, _) => {
            for arg in args {
                collect_expr_dependencies(arg, dependencies);
            }
        }
        ast::Expr::ArrayAccess(array, index, _) => {
            collect_expr_dependencies(array, dependencies);
            collect_expr_dependencies(index, dependencies);
        }
        ast::Expr::FieldAccess(obj, _, _) => {
            collect_expr_dependencies(obj, dependencies);
        }
        ast::Expr::ArrayInit(elements, _) => {
            for element in elements {
                collect_expr_dependencies(element, dependencies);
            }
        }
        ast::Expr::StructInit(_, fields, _) => {
            for (_, expr) in fields {
                collect_expr_dependencies(expr, dependencies);
            }
        }
        ast::Expr::Cast(expr, _, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Expr::Assign(left, right, _) => {
            collect_expr_dependencies(left, dependencies);
            collect_expr_dependencies(right, dependencies);
        }
        ast::Expr::Deref(expr, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Expr::Range(start, end, _, _) => {
            collect_expr_dependencies(start, dependencies);
            collect_expr_dependencies(end, dependencies);
        }
        ast::Expr::EnumConstruct(_, _, args, _) => {
            for arg in args {
                collect_expr_dependencies(arg, dependencies);
            }
        }

        ast::Expr::Int(_, _)
        | ast::Expr::Int64(_, _)
        | ast::Expr::F32(_, _)
        | ast::Expr::Bool(_, _)
        | ast::Expr::Str(_, _)
        | ast::Expr::Void(_)
        | ast::Expr::None(_)
        | ast::Expr::InfiniteRange(_, _) => {}
        ast::Expr::SafeBlock(stmts, _) => {
            collect_variable_dependencies_from_block(stmts, dependencies);
        }
        ast::Expr::TemplateStr(parts, _) => {
            for part in parts {
                if let ast::TemplateStrPart::Expression(expr) = part {
                    collect_expr_dependencies(expr, dependencies);
                }
            }
        }
        ast::Expr::Match(_, arms, _) => {
            for arm in arms {
                match &arm.body {
                    ast::MatchArmBody::Expr(expr) => {
                        collect_expr_dependencies(expr, dependencies);
                    }
                    ast::MatchArmBody::Block(stmts) => {
                        for stmt in stmts {
                            collect_variable_dependencies(stmt, dependencies);
                        }
                    }
                }
            }
        }
        ast::Expr::If(condition, then_branch, else_branch, _) => {
            collect_expr_dependencies(condition, dependencies);
            for stmt in then_branch {
                collect_variable_dependencies(stmt, dependencies);
            }
            if let Some(else_stmts) = else_branch {
                for stmt in else_stmts {
                    collect_variable_dependencies(stmt, dependencies);
                }
            }
        }
        ast::Expr::Loop(body, _) => {
            for stmt in body {
                collect_variable_dependencies(stmt, dependencies);
            }
        }
    }
}

#[derive(Debug)]
pub struct ModuleCompiler {
    module_graph: ModuleGraph,
    build_cache: BuildCacheManager,
}

impl ModuleCompiler {
    pub fn new(build_dir: &Path) -> Self {
        Self {
            module_graph: ModuleGraph::new(),
            build_cache: BuildCacheManager::new(build_dir),
        }
    }

    pub fn initialize(&mut self) -> Result<()> {
        self.build_cache.initialize()
    }

    pub fn add_module(&mut self, module_path: &Path) -> Result<()> {
        let module_info = ModuleInfo::new(module_path.to_path_buf())?;
        self.module_graph.add_module(module_info);
        Ok(())
    }

    pub fn compile_module_incremental(
        &mut self,
        files: &mut Files<String>,
        module_path: &Path,
        verbose: bool,
    ) -> Result<bool> {
        if self.build_cache.is_module_cached(module_path)? {
            if verbose {
                println!("Module {} is up to date", module_path.display());
            }
            return Ok(false);
        }

        let needs_compilation = {
            if let Some(module_info) = self.module_graph.modules.get_mut(module_path) {
                module_info.update_if_changed()? || module_info.is_dirty
            } else {
                false
            }
        };

        if needs_compilation {
            self.build_cache.invalidate_entry(module_path)?;
            let invalidated = self.build_cache.invalidate_dependents(module_path)?;
            
            for dependent in &invalidated {
                self.module_graph.mark_dirty_cascade(dependent);
            }

            if verbose {
                println!("Recompiling module: {}", module_path.display());
            }

            let content = fs::read_to_string(module_path)?;
            let file_id = files.add(module_path.to_string_lossy().to_string(), content);
            
            let lexer = crate::lexer::Lexer::new(files, file_id);
            let mut parser = crate::parser::Parser::new(lexer);
            let program = parser.parse().map_err(|e| anyhow!("Parse error: {:?}", e))?;

            if let Some(module_info) = self.module_graph.modules.get_mut(module_path) {
                module_info.program = Some(program.clone());
                module_info.is_dirty = false;
            }

            let dependencies = self.collect_module_dependencies(&program.imports, module_path)?;
            self.build_cache.cache_module(module_path, dependencies, Vec::new())?;

            return Ok(true);
        }

        Ok(false)
    }

    pub fn compile_all_modules(
        &mut self,
        files: &mut Files<String>,
        verbose: bool,
    ) -> Result<Vec<PathBuf>> {
        let mut compiled_modules = Vec::new();
        let sorted_modules = self.module_graph.topological_sort()?;

        for module_path in sorted_modules {
            if self.compile_module_incremental(files, &module_path, verbose)? {
                compiled_modules.push(module_path);
            }
        }

        Ok(compiled_modules)
    }

    pub fn collect_module_dependencies(
        &mut self,
        imports: &[ast::ImportDeclaration],
        base_path: &Path,
    ) -> Result<Vec<PathBuf>> {
        let resolved_imports = resolve_imports_only(imports, base_path)?;
        Ok(resolved_imports.into_iter().map(|r| r.path).collect())
    }

    pub fn cache_compilation_artifacts(
        &mut self,
        module_path: &Path,
        dependencies: Vec<PathBuf>,
        artifacts: Vec<PathBuf>,
    ) -> Result<()> {
        self.build_cache.cache_module(module_path, dependencies, artifacts)
    }

    pub fn discover_all_modules(&mut self, entry_point: &Path) -> Result<()> {

        self.auto_load_stdlib_modules()?;
        
        let mut to_process = vec![entry_point.to_path_buf()];
        let mut processed = std::collections::HashSet::new();

        while let Some(current_path) = to_process.pop() {
            let normalized_current = current_path.canonicalize().unwrap_or(current_path.clone());
            if processed.contains(&normalized_current) {
                continue;
            }
            processed.insert(normalized_current.clone());

            if !self.module_graph.modules.contains_key(&normalized_current) {
                self.add_module(&normalized_current)?;
            }

            let content = fs::read_to_string(&normalized_current)?;
            let mut files = Files::<String>::new();
            let file_id = files.add(normalized_current.to_string_lossy().to_string(), content);
            
            let lexer = crate::lexer::Lexer::new(&files, file_id);
            let mut parser = crate::parser::Parser::new(lexer);
            let program = parser.parse().map_err(|e| anyhow!("Parse error in {}: {:?}", normalized_current.display(), e))?;

            let resolved_deps = resolve_imports_only(&program.imports, &normalized_current)?;
            
            for resolved_import in &resolved_deps {
                let dep_path = &resolved_import.path;
                let dep_normalized = dep_path.canonicalize().unwrap_or_else(|_| dep_path.clone());
                if !processed.contains(&dep_normalized) {
                    to_process.push(dep_normalized.clone());
                }
                self.module_graph.add_dependency(&normalized_current, &dep_normalized)?;
            }

            if let Some(module_info) = self.module_graph.modules.get_mut(&normalized_current) {
                module_info.imports = program.imports.clone();
                module_info.program = Some(program);
            }
        }

        Ok(())
    }

    pub fn create_merged_program(&self, entry_point: &Path) -> Result<ast::Program> {
        let normalized_entry_point = entry_point.canonicalize().unwrap_or_else(|_| entry_point.to_path_buf());
        let build_order = self.module_graph.topological_sort()?;
        let mut merged = ast::Program {
            imports: Vec::new(),
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            ffi_functions: Vec::new(),
            ffi_variables: Vec::new(),
            stmts: Vec::new(),
            impls: Vec::new(),
            tests: Vec::new(),
        };

        let mut modified_programs: HashMap<PathBuf, ast::Program> = HashMap::new();
        
        for module_path in &build_order {
            if let Some(module_info) = self.module_graph.modules.get(module_path) {
                if let Some(mut program) = module_info.program.clone() {
                    if module_path != &normalized_entry_point {
                        self.analyze_and_mark_module_dependencies(&mut program);
                        modified_programs.insert(module_path.clone(), program.clone());
                    }
                    
                    if module_path == &normalized_entry_point {
                        merged.stmts.extend(program.stmts.clone());
                        merged.functions.extend(program.functions.clone());
                        merged.structs.extend(program.structs.clone());
                        merged.enums.extend(program.enums.clone());
                        merged.ffi_functions.extend(program.ffi_functions.clone());
                        merged.ffi_variables.extend(program.ffi_variables.clone());
                        merged.impls.extend(program.impls.clone());
                        merged.imports.extend(program.imports.clone());
                        merged.tests.extend(program.tests.clone());
                    } else {
                        for function in &program.functions {
                            if matches!(function.visibility, ast::Visibility::Public | ast::Visibility::Internal) {
                                merged.functions.push(function.clone());
                            }
                        }
                        for struct_def in &program.structs {
                            if matches!(struct_def.visibility, ast::Visibility::Public) {
                                merged.structs.push(struct_def.clone());
                            }
                        }
                        for enum_def in &program.enums {
                            if matches!(enum_def.visibility, ast::Visibility::Public) {
                                merged.enums.push(enum_def.clone());
                            }
                        }
                        for (i, stmt) in program.stmts.iter().enumerate() {
                            match stmt {
                                ast::Stmt::Let(name, _, _, _, visibility) => {
                                    if matches!(visibility, ast::Visibility::Public | ast::Visibility::Internal) {
                                        merged.stmts.push(stmt.clone());
                                    }
                                }
                                ast::Stmt::Block(block_stmts, _) => {
                                    for block_stmt in block_stmts {
                                        if let ast::Stmt::Let(name, _, _, _, visibility) = block_stmt {
                                            if matches!(visibility, ast::Visibility::Public | ast::Visibility::Internal) {
                                                merged.stmts.push(block_stmt.clone());
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                        merged.ffi_functions.extend(program.ffi_functions.clone());
                        merged.ffi_variables.extend(program.ffi_variables.clone());
                        merged.impls.extend(program.impls.clone());
                        merged.tests.extend(program.tests.clone());
                    }
                }
            }
        }

        let mut seen_functions = std::collections::HashSet::new();
        merged.functions.retain(|func| {
            if seen_functions.contains(&func.name) {
                false
            } else {
                seen_functions.insert(func.name.clone());
                true
            }
        });

        Ok(merged)
    }

    pub fn get_imported_info(&self) -> Result<(
        std::collections::HashMap<String, (Vec<crate::ast::Type>, crate::ast::Type)>,
        Vec<crate::ast::StructDef>,
        Vec<crate::ast::FfiVariable>,
    )> {
        let mut imported_functions = std::collections::HashMap::new();
        let mut imported_structs = Vec::new();
        let mut imported_ffi_vars = Vec::new();

        for (_, module_info) in &self.module_graph.modules {
            if let Some(program) = &module_info.program {
                for function in &program.functions {
                    if matches!(function.visibility, crate::ast::Visibility::Public | crate::ast::Visibility::Internal) {
                        let params: Vec<crate::ast::Type> = function.params.iter().map(|(_, t)| t.clone()).collect();
                        imported_functions.insert(function.name.clone(), (params, function.return_type.clone()));
                    }
                }

                for struct_def in &program.structs {
                    if matches!(struct_def.visibility, crate::ast::Visibility::Public) {
                        imported_structs.push(struct_def.clone());
                    }
                }

                imported_ffi_vars.extend(program.ffi_variables.clone());
            }
        }

        Ok((imported_functions, imported_structs, imported_ffi_vars))
    }

    pub fn get_entry_file_id(&self, files: &mut codespan::Files<String>, entry_path: &Path) -> Result<codespan::FileId> {
        let content = std::fs::read_to_string(entry_path)?;
        let file_id = files.add(entry_path.to_string_lossy().to_string(), content);
        Ok(file_id)
    }

    pub fn auto_load_stdlib_modules(&mut self) -> Result<()> {
        let lib_dir = get_lib_path()?;
        let std_src_dir = lib_dir.join("std").join("src");
        
        if !std_src_dir.exists() {
            return Ok(());
        }
        
        let std_modules = ["core.ve", "io.ve", "testing.ve"];
        
        for module_name in &std_modules {
            let module_path = std_src_dir.join(module_name);
            if !module_path.exists() {
                continue;
            }
            
            let normalized_path = module_path.canonicalize().unwrap_or(module_path.clone());
            
            if self.module_graph.modules.contains_key(&normalized_path) {
                if let Some(module_info) = self.module_graph.modules.get(&normalized_path) {
                    if module_info.program.is_some() {
                        continue;
                    }
                }
            } else {
                self.add_module(&normalized_path)?;
            }
                
            let content = fs::read_to_string(&normalized_path)?;
            let mut files = Files::<String>::new();
            let file_id = files.add(normalized_path.to_string_lossy().to_string(), content);
                
            let lexer = crate::lexer::Lexer::new(&files, file_id);
            let mut parser = crate::parser::Parser::new(lexer);
            let program = parser.parse().map_err(|e| anyhow!("Parse error in {}: {:?}", normalized_path.display(), e))?;
                
            let resolved_deps = resolve_imports_only(&program.imports, &normalized_path)?;
                
            for resolved_import in &resolved_deps {
                let dep_path = &resolved_import.path;
                let dep_normalized = dep_path.canonicalize().unwrap_or_else(|_| dep_path.clone());
                if !self.module_graph.modules.contains_key(&dep_normalized) {
                    self.add_module(&dep_normalized)?;
                }
                self.module_graph.add_dependency(&normalized_path, &dep_normalized)?;
            }
                
            if let Some(module_info) = self.module_graph.modules.get_mut(&normalized_path) {
                module_info.imports = program.imports.clone();
                module_info.program = Some(program);
            }
        }
        
        Ok(())
    }

    fn analyze_and_mark_module_dependencies(&self, program: &mut ast::Program) {
        let mut needed_variables: HashSet<String> = HashSet::new();
        for function in &program.functions {
            if matches!(function.visibility, ast::Visibility::Public) {
                let mut function_deps = HashSet::new();
                collect_variable_dependencies_from_block(&function.body, &mut function_deps);
                needed_variables.extend(function_deps);
            }
        }

        fn mark_variables_recursive(stmts: &mut [ast::Stmt], needed_vars: &HashSet<String>) {
            for stmt in stmts {
                match stmt {
                    ast::Stmt::Let(name, _ty, _expr, _span, visibility) => {
                        if needed_vars.contains(name) && matches!(visibility, ast::Visibility::Private)
                        {
                            *visibility = ast::Visibility::Internal;
                        }
                    }
                    ast::Stmt::Block(block_stmts, _) => {
                        mark_variables_recursive(block_stmts, needed_vars);
                    }
                    _ => {}
                }
            }
        }
        mark_variables_recursive(&mut program.stmts, &needed_variables);
    }

}

#[derive(Debug, Clone)]
pub struct ResolvedImport {
    pub path: PathBuf,
    pub import_type: ImportType,
    pub module_path: String,
}

#[derive(Debug, Clone)]
pub enum ImportType {
    All { alias: Option<String> },
    Specifiers { specifiers: Vec<ast::ImportSpecifier> },
}

#[derive(Debug, Clone)]
pub struct CacheConfig {
    pub excluded_dirs: Vec<String>,
    pub cache_lib_modules: bool,
    pub cache_std_modules: bool,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            excluded_dirs: vec![
                "examples".to_string(),
                "test".to_string(), 
                "tests".to_string(),
                "bench".to_string(),
                "benchmarks".to_string(),
            ],
            cache_lib_modules: true,
            cache_std_modules: true,
        }
    }
}
