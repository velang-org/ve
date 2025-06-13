use anyhow::{Context, Result};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::fs;
use sha2::{Sha256, Digest};

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolInfo {
    pub name: String,
    pub symbol_type: SymbolType,
    pub signature_hash: String,
    pub visibility: crate::ast::Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Function {
        params: Vec<crate::ast::Type>,
        return_type: crate::ast::Type,
    },
    Struct {
        fields: Vec<(String, crate::ast::Type)>,
    },
}

impl SymbolInfo {
    pub fn from_function(func: &crate::ast::Function) -> Self {
        let params: Vec<crate::ast::Type> = func.params.iter().map(|(_, t)| t.clone()).collect();
        let symbol_type = SymbolType::Function {
            params: params.clone(),
            return_type: func.return_type.clone(),
        };
        
        let signature = format!("{}({:?})->{:?}", func.name, params, func.return_type);
        let signature_hash = Self::calculate_signature_hash(&signature);
        
        Self {
            name: func.name.clone(),
            symbol_type,
            signature_hash,
            visibility: func.visibility.clone(),
        }
    }
    
    pub fn from_struct(struct_def: &crate::ast::StructDef) -> Self {
        let fields: Vec<(String, crate::ast::Type)> = struct_def.fields.iter()
            .map(|f| (f.name.clone(), f.ty.clone()))
            .collect();
        let symbol_type = SymbolType::Struct { fields: fields.clone() };
        
        let signature = format!("struct {}{{ {:?} }}", struct_def.name, fields);
        let signature_hash = Self::calculate_signature_hash(&signature);
        
        Self {
            name: struct_def.name.clone(),
            symbol_type,
            signature_hash,
            visibility: struct_def.visibility.clone(),
        }
    }  
    
    fn calculate_signature_hash(signature: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(signature.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

#[derive(Debug, Clone)]
pub struct SymbolDependencyGraph {
    pub symbol_dependencies: HashMap<String, HashSet<String>>,
    pub symbol_dependents: HashMap<String, HashSet<String>>,
}

impl SymbolDependencyGraph {
    pub fn new() -> Self {
        Self {
            symbol_dependencies: HashMap::new(),
            symbol_dependents: HashMap::new(),
        }
    }
    
    pub fn add_dependency(&mut self, from_symbol: &str, to_symbol: &str) {
        self.symbol_dependencies
            .entry(from_symbol.to_string())
            .or_insert_with(HashSet::new)
            .insert(to_symbol.to_string());
            
        self.symbol_dependents
            .entry(to_symbol.to_string())
            .or_insert_with(HashSet::new)
            .insert(from_symbol.to_string());
    }
    
    pub fn get_affected_symbols(&self, changed_symbol: &str) -> HashSet<String> {
        let mut affected = HashSet::new();
        let mut to_check = vec![changed_symbol.to_string()];
        
        while let Some(symbol) = to_check.pop() {
            if affected.insert(symbol.clone()) {
                if let Some(dependents) = self.symbol_dependents.get(&symbol) {
                    for dependent in dependents {
                        if !affected.contains(dependent) {
                            to_check.push(dependent.clone());
                        }
                    }
                }
            }
        }
        
        affected
    }
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub file_path: PathBuf,
    pub content_hash: String,
    pub last_modified: SystemTime,
    pub imports: Vec<crate::ast::ImportDeclaration>,
    pub program: Option<crate::ast::Program>,
    pub is_dirty: bool,
    pub exported_symbols: HashMap<String, SymbolInfo>,
    pub imported_symbols: HashMap<String, String>,
    pub symbol_changes: HashSet<String>,
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
            exported_symbols: HashMap::new(),
            imported_symbols: HashMap::new(),
            symbol_changes: HashSet::new(),
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
                  if let Some(old_program) = self.program.clone() {
                    self.detect_symbol_changes(&old_program);
                }
                
                return Ok(true);
            }
        }
        
        Ok(false)
    }
      pub fn extract_symbols(&mut self, program: &crate::ast::Program) {
        self.exported_symbols.clear();
        
        for function in &program.functions {
            if matches!(function.visibility, crate::ast::Visibility::Public) {
                let symbol_info = SymbolInfo::from_function(function);
                self.exported_symbols.insert(function.name.clone(), symbol_info);
            }
        }
        
        for struct_def in &program.structs {
            if matches!(struct_def.visibility, crate::ast::Visibility::Public) {
                let symbol_info = SymbolInfo::from_struct(struct_def);
                self.exported_symbols.insert(struct_def.name.clone(), symbol_info);
            }
        }
        
    }
    
    fn detect_symbol_changes(&mut self, old_program: &crate::ast::Program) {
        self.symbol_changes.clear();
        
        let mut old_symbols = HashMap::new();
        
        for function in &old_program.functions {
            if matches!(function.visibility, crate::ast::Visibility::Public) {
                let symbol_info = SymbolInfo::from_function(function);
                old_symbols.insert(function.name.clone(), symbol_info);
            }
        }        for struct_def in &old_program.structs {
            if matches!(struct_def.visibility, crate::ast::Visibility::Public) {
                let symbol_info = SymbolInfo::from_struct(struct_def);
                old_symbols.insert(struct_def.name.clone(), symbol_info);
            }
        }
        
    
        for (name, new_symbol) in &self.exported_symbols {
            if let Some(old_symbol) = old_symbols.get(name) {
                if old_symbol.signature_hash != new_symbol.signature_hash {
                    self.symbol_changes.insert(name.clone());
                }
            } else {
                self.symbol_changes.insert(name.clone());
            }
        }
        
        for name in old_symbols.keys() {
            if !self.exported_symbols.contains_key(name) {
                self.symbol_changes.insert(name.clone());
            }
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CacheEntry {
    pub source_hash: String,
    pub dependencies: Vec<PathBuf>,
    pub output_files: Vec<PathBuf>,
    pub timestamp: SystemTime,
}

impl CacheEntry {
    pub fn new(
        source_hash: String,
        dependencies: Vec<PathBuf>,
        output_files: Vec<PathBuf>,
    ) -> Self {
        Self {
            source_hash,
            dependencies,
            output_files,
            timestamp: SystemTime::now(),
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SymbolCacheEntry {
    pub symbol_hash: String,
    pub dependencies: Vec<String>,
    pub compilation_result: Option<String>,
    pub timestamp: SystemTime,
}

impl SymbolCacheEntry {
    pub fn new(symbol_hash: String, dependencies: Vec<String>) -> Self {
        Self {
            symbol_hash,
            dependencies,
            compilation_result: None,
            timestamp: SystemTime::now(),
        }
    }
}

#[derive(Debug)]
pub struct BuildCacheManager {
    cache_entries: HashMap<PathBuf, CacheEntry>,
    symbol_cache: HashMap<String, SymbolCacheEntry>,
    cache_file: PathBuf,
    symbol_cache_file: PathBuf,
}

impl BuildCacheManager {    pub fn new(build_dir: &Path) -> Self {
        let cache_file = build_dir.join(".veil_cache.json");
        let symbol_cache_file = build_dir.join(".veil_symbol_cache.json");
        let mut manager = Self {
            cache_entries: HashMap::new(),
            symbol_cache: HashMap::new(),
            cache_file,
            symbol_cache_file,
        };
        
        let _ = manager.load_cache();
        let _ = manager.load_symbol_cache();
        manager
    }
    
    fn load_cache(&mut self) -> Result<()> {
        if !self.cache_file.exists() {
            return Ok(());
        }
        
        let content = fs::read_to_string(&self.cache_file)?;
        self.cache_entries = serde_json::from_str(&content)
            .with_context(|| "Failed to parse cache file")?;
        Ok(())
    }
    
    fn save_cache(&self) -> Result<()> {
        if let Some(parent) = self.cache_file.parent() {
            fs::create_dir_all(parent)?;
        }
        
        let content = serde_json::to_string_pretty(&self.cache_entries)?;
        fs::write(&self.cache_file, content)?;
        Ok(())
    }
    
    fn load_symbol_cache(&mut self) -> Result<()> {
        if !self.symbol_cache_file.exists() {
            return Ok(());
        }
        
        let content = fs::read_to_string(&self.symbol_cache_file)?;
        self.symbol_cache = serde_json::from_str(&content)
            .with_context(|| "Failed to parse symbol cache file")?;
        Ok(())
    }
    
    fn save_symbol_cache(&self) -> Result<()> {
        if let Some(parent) = self.symbol_cache_file.parent() {
            fs::create_dir_all(parent)?;
        }
        
        let content = serde_json::to_string_pretty(&self.symbol_cache)?;
        fs::write(&self.symbol_cache_file, content)?;
        Ok(())
    }

    pub fn cache_module(
        &mut self,
        source_path: &Path,
        dependencies: Vec<PathBuf>,
        output_files: Vec<PathBuf>,
    ) -> Result<()> {
        let source_hash = ModuleInfo::calculate_hash(&fs::read_to_string(source_path)?);
        let entry = CacheEntry::new(source_hash, dependencies, output_files);
        self.cache_entries.insert(source_path.to_path_buf(), entry);
        self.save_cache()?;
        Ok(())
    }

    pub fn is_cache_valid(&self, source_path: &Path) -> Result<bool> {
        let Some(entry) = self.cache_entries.get(source_path) else {
            return Ok(false);
        };

        if !source_path.exists() {
            return Ok(false);
        }
        
        let current_hash = ModuleInfo::calculate_hash(&fs::read_to_string(source_path)?);
        if current_hash != entry.source_hash {
            return Ok(false);
        }
        for dep_path in &entry.dependencies {
            if !dep_path.exists() {
                return Ok(false);
            }

            let dep_metadata = fs::metadata(dep_path)?;
            let dep_modified = dep_metadata.modified()?;
            
            if dep_modified > entry.timestamp {
                return Ok(false);
            }
        }

        Ok(true)
    }

    pub fn invalidate_cache(&mut self, source_path: &Path) {
        if self.cache_entries.remove(source_path).is_some() {
            let _ = self.save_cache();
        }
    }

    pub fn cache_symbol(
        &mut self,
        symbol_id: &str,
        symbol_hash: String,
        dependencies: Vec<String>,
    ) -> Result<()> {
        let entry = SymbolCacheEntry::new(symbol_hash, dependencies);
        self.symbol_cache.insert(symbol_id.to_string(), entry);
        self.save_symbol_cache()?;
        Ok(())
    }

    pub fn is_symbol_cache_valid(&self, symbol_id: &str, current_hash: &str) -> bool {
        if let Some(entry) = self.symbol_cache.get(symbol_id) {
            entry.symbol_hash == current_hash
        } else {
            false
        }
    }

    pub fn invalidate_symbol_cache(&mut self, symbol_id: &str) {
        if self.symbol_cache.remove(symbol_id).is_some() {
            let _ = self.save_symbol_cache();
        }
    }

    pub fn get_symbol_dependencies(&self, symbol_id: &str) -> Vec<String> {
        self.symbol_cache
            .get(symbol_id)
            .map(|entry| entry.dependencies.clone())
            .unwrap_or_default()
    }
}
