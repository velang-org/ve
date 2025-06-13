use super::cache::{ModuleInfo, BuildCacheManager, SymbolDependencyGraph};
use crate::ast;
use anyhow::{Result, anyhow};
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct ModuleGraph {
    pub modules: HashMap<PathBuf, ModuleInfo>,
    pub dependencies: HashMap<PathBuf, HashSet<PathBuf>>,
    pub dependents: HashMap<PathBuf, HashSet<PathBuf>>,
    pub symbol_graph: SymbolDependencyGraph,
}

impl ModuleGraph {    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            dependencies: HashMap::new(),
            dependents: HashMap::new(),
            symbol_graph: SymbolDependencyGraph::new(),
        }
    }

    pub fn add_module(&mut self, module_info: ModuleInfo) {
        let path = module_info.file_path.clone();
        self.modules.insert(path.clone(), module_info);
        self.dependencies.entry(path.clone()).or_insert_with(HashSet::new);
        self.dependents.entry(path).or_insert_with(HashSet::new);
    }    pub fn add_dependency(&mut self, from: &Path, to: &Path) -> Result<()> {
        let from_canonical = from.canonicalize()
            .map_err(|_| anyhow!("Failed to canonicalize 'from' path: {}", from.display()))?;
        let to_canonical = to.canonicalize()
            .map_err(|_| anyhow!("Failed to canonicalize 'to' path: {}", to.display()))?;

        if from_canonical == to_canonical {
            return Ok(()); 
        }

        self.dependencies
            .entry(from_canonical.clone())
            .or_insert_with(HashSet::new)
            .insert(to_canonical.clone());

        self.dependents
            .entry(to_canonical)
            .or_insert_with(HashSet::new)
            .insert(from_canonical);

        Ok(())    }

    pub fn topological_sort(&self) -> Result<Vec<PathBuf>> {
        let mut in_degree: HashMap<PathBuf, usize> = HashMap::new();
        let mut result = Vec::new();
        let mut queue = VecDeque::new();

        for module_path in self.modules.keys() {
            in_degree.insert(module_path.clone(), 0);
        }

        for deps in self.dependencies.values() {
            for dep in deps {
                *in_degree.entry(dep.clone()).or_insert(0) += 1;
            }
        }

        for (module_path, &degree) in &in_degree {
            if degree == 0 {
                queue.push_back(module_path.clone());
            }
        }

        while let Some(current) = queue.pop_front() {
            result.push(current.clone());

            if let Some(deps) = self.dependencies.get(&current) {
                for dep in deps {
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
            for module_path in self.modules.keys() {
                if !result.contains(module_path) {
                    result.push(module_path.clone());
                }
            }
        }

        Ok(result)
    }

    pub fn get_dependencies(&self, module_path: &Path) -> Vec<PathBuf> {
        self.dependencies
            .get(module_path)
            .map(|deps| deps.iter().cloned().collect())
            .unwrap_or_default()
    }

    pub fn mark_dirty_cascade(&mut self, changed_module: &Path) {
        let mut to_mark_dirty = VecDeque::new();
        let mut visited = HashSet::new();

        to_mark_dirty.push_back(changed_module.to_path_buf());

        while let Some(current) = to_mark_dirty.pop_front() {
            if !visited.insert(current.clone()) {
                continue;
            }

            if let Some(module) = self.modules.get_mut(&current) {
                module.is_dirty = true;
            }

            if let Some(dependents) = self.dependents.get(&current) {
                for dependent in dependents {
                    to_mark_dirty.push_back(dependent.clone());
                }
            }
        }
    }
    
    pub fn mark_symbol_changes_cascade(&mut self, changed_module: &Path, changed_symbols: &HashSet<String>) {
        if changed_symbols.is_empty() {
            return;
        }
        
        let mut affected_symbols = HashSet::new();
        
        for symbol in changed_symbols {
            let symbol_id = format!("{}::{}", changed_module.display(), symbol);
            let symbol_affected = self.symbol_graph.get_affected_symbols(&symbol_id);
            affected_symbols.extend(symbol_affected);
        }
        
        let mut affected_modules = HashSet::new();
        for symbol_id in &affected_symbols {
            if let Some(module_path) = symbol_id.split("::").next() {
                if let Ok(path) = PathBuf::from(module_path).canonicalize() {
                    affected_modules.insert(path);
                }
            }
        }
        
        for module_path in affected_modules {
            if let Some(module) = self.modules.get_mut(&module_path) {
                module.is_dirty = true;
            }
        }
    }
    
    pub fn build_symbol_dependencies(&mut self, module_path: &Path, program: &crate::ast::Program) {
        let module_prefix = format!("{}::", module_path.display());
        
        for function in &program.functions {
            let symbol_id = format!("{}{}", module_prefix, function.name);
            
            self.extract_symbol_dependencies(&symbol_id, &function.body);
        }
    }
      fn extract_symbol_dependencies(&mut self, symbol_id: &str, statements: &[crate::ast::Stmt]) {
        for stmt in statements {
            match stmt {
                crate::ast::Stmt::Expr(expr, _) => {
                    self.extract_expr_dependencies(symbol_id, expr);
                }
                crate::ast::Stmt::If(_, then_stmts, else_stmts, _) => {
                    self.extract_symbol_dependencies(symbol_id, then_stmts);
                    if let Some(else_block) = else_stmts {
                        self.extract_symbol_dependencies(symbol_id, else_block);
                    }
                }
                crate::ast::Stmt::While(_, body, _) => {
                    self.extract_symbol_dependencies(symbol_id, body);
                }
                crate::ast::Stmt::For(_, _, _, _, body, _) => {
                    self.extract_symbol_dependencies(symbol_id, body);
                }
                crate::ast::Stmt::Block(body, _) => {
                    self.extract_symbol_dependencies(symbol_id, body);
                }
                _ => {}
            }
        }
    }
    
    fn extract_expr_dependencies(&mut self, symbol_id: &str, expr: &crate::ast::Expr) {
        match expr {
            crate::ast::Expr::Call(name, _, _) => {
                let dependency_id = format!("external::{}", name);
                self.symbol_graph.add_dependency(symbol_id, &dependency_id);
            }
            _ => {}
        }
    }

    pub fn optimize_cycle_dependencies(&mut self) -> Result<()> {
        let cycles = self.detect_cycles()?;
        
        for cycle in cycles {
            self.minimize_cycle_rebuilds(&cycle)?;
        }
        
        Ok(())
    }
    
    fn detect_cycles(&self) -> Result<Vec<Vec<PathBuf>>> {
        let mut cycles = Vec::new();
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut path = Vec::new();
        
        for module_path in self.modules.keys() {
            if !visited.contains(module_path) {
                self.dfs_cycle_detection(
                    module_path,
                    &mut visited,
                    &mut rec_stack,
                    &mut path,
                    &mut cycles,
                );
            }
        }
        
        Ok(cycles)
    }
    
    fn dfs_cycle_detection(
        &self,
        current: &Path,
        visited: &mut HashSet<PathBuf>,
        rec_stack: &mut HashSet<PathBuf>,
        path: &mut Vec<PathBuf>,
        cycles: &mut Vec<Vec<PathBuf>>,
    ) {
        visited.insert(current.to_path_buf());
        rec_stack.insert(current.to_path_buf());
        path.push(current.to_path_buf());
        
        if let Some(deps) = self.dependencies.get(current) {
            for dep in deps {
                if !visited.contains(dep) {
                    self.dfs_cycle_detection(dep, visited, rec_stack, path, cycles);
                } else if rec_stack.contains(dep) {
                    if let Some(cycle_start) = path.iter().position(|p| p == dep) {
                        let cycle = path[cycle_start..].to_vec();
                        cycles.push(cycle);
                    }
                }
            }
        }
        
        path.pop();
        rec_stack.remove(&current.to_path_buf());
    }
      fn minimize_cycle_rebuilds(&mut self, cycle: &[PathBuf]) -> Result<()> {
        let mut interface_stable = HashMap::new();
        
        for module_path in cycle {
            if let Some(module_info) = self.modules.get(module_path) {
                let has_interface_changes = module_info.exported_symbols.iter()
                    .any(|(name, _)| module_info.symbol_changes.contains(name));
                    
                interface_stable.insert(module_path.clone(), !has_interface_changes);
            }
        }
          let stable_modules: Vec<_> = interface_stable.iter()
            .filter(|(_, is_stable)| **is_stable)
            .map(|(path, _)| path.clone())
            .collect();
        
        for stable_module_path in stable_modules {
            if let Some(module_info) = self.modules.get(&stable_module_path) {
                let symbol_changes = module_info.symbol_changes.clone();
                
                for dependent_path in cycle {
                    if dependent_path != &stable_module_path {
                        if let Some(dependent_module) = self.modules.get_mut(dependent_path) {
                            let depends_on_changed_interface = dependent_module.imported_symbols.iter()
                                .any(|(symbol, source)| {
                                    source == &stable_module_path.to_string_lossy().to_string() && 
                                    symbol_changes.contains(symbol)
                                });
                            
                            if !depends_on_changed_interface {
                                dependent_module.is_dirty = false;
                            }
                        }
                    }
                }
            }
        }
        
        Ok(())
    }
}

#[derive(Debug)]
pub struct IncrementalCompiler {
    pub module_graph: ModuleGraph,
    pub build_cache: BuildCacheManager,
}

impl IncrementalCompiler {
    pub fn new(build_dir: &Path) -> Self {
        Self {
            module_graph: ModuleGraph::new(),
            build_cache: BuildCacheManager::new(build_dir),
        }
    }    pub fn check_module_for_changes(&mut self, module_path: &Path) -> Result<bool> {
        if let Some(module_info) = self.module_graph.modules.get_mut(module_path) {
            let changed = module_info.update_if_changed()?;
            if changed {
                self.build_cache.invalidate_cache(module_path);
                  if let Some(program) = module_info.program.clone() {
                    module_info.extract_symbols(&program);
                    let changed_symbols = &module_info.symbol_changes.clone();
                    
                    if !changed_symbols.is_empty() {
                        self.module_graph.mark_symbol_changes_cascade(module_path, changed_symbols);
                        
                        for symbol in changed_symbols {
                            let symbol_id = format!("{}::{}", module_path.display(), symbol);
                            self.build_cache.invalidate_symbol_cache(&symbol_id);
                        }
                    } else {
                        self.module_graph.mark_dirty_cascade(module_path);
                    }
                } else {
                    self.module_graph.mark_dirty_cascade(module_path);
                }
            }
            Ok(changed)
        } else {
            let module_info = ModuleInfo::new(module_path.to_path_buf())?;
            self.module_graph.add_module(module_info);
            Ok(true)
        }
    }

    pub fn collect_module_dependencies(
        &mut self,
        imports: &[ast::ImportDeclaration],
        base_path: &Path,
    ) -> Result<Vec<PathBuf>> {
        let resolved_imports = super::resolve_imports_only(imports, base_path)?;
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

    pub fn build_dependency_graph(&mut self, entry_point: &Path) -> Result<()> {
        let mut to_process = Vec::new();
        let mut processed = std::collections::HashSet::new();

        to_process.push(entry_point.to_path_buf());

        while let Some(current_path) = to_process.pop() {
            let normalized_current = current_path.canonicalize()
                .unwrap_or_else(|_| current_path.clone());

            if processed.contains(&normalized_current) {
                continue;
            }
            processed.insert(normalized_current.clone());            let content = std::fs::read_to_string(&normalized_current)?;
            let mut files = codespan::Files::<String>::new();
            let file_id = files.add(normalized_current.to_string_lossy().to_string(), content);
            
            let lexer = crate::lexer::Lexer::new(&files, file_id);
            let mut parser = crate::parser::Parser::new(lexer);
            let mut program = parser.parse().map_err(|e| {
                crate::helpers::print_parse_error(&files, file_id, &e, &normalized_current);
                let line_col = crate::helpers::extract_line_col_from_error(&files, file_id, &e);
                let clean_path = normalized_current.to_string_lossy().replace("\\\\?\\", "");
                anyhow::anyhow!("{}:{}: {}", clean_path, line_col, e.message)            
            })?;      
            let is_prelude_module = normalized_current.to_string_lossy().ends_with("prelude.ve");
            let has_prelude_import = program.imports.iter().any(|import| {
                match import {
                    crate::ast::ImportDeclaration::ImportAll { module_path, .. } => module_path == "std/prelude",
                    crate::ast::ImportDeclaration::ExportImportAll { module_path, .. } => module_path == "std/prelude",
                    _ => false,
                }
            });

            if !is_prelude_module && !has_prelude_import {
                let prelude_import = crate::ast::ImportDeclaration::ImportAll {
                    module_path: "std/prelude".to_string(),
                    module_type: crate::ast::ModuleType::Standard,
                    alias: None,
                };
                program.imports.insert(0, prelude_import);
            }let resolved_deps = super::resolve_imports_only(&program.imports, &normalized_current)?;
            
            for resolved_import in &resolved_deps {
                let dep_path = &resolved_import.path;
                let dep_normalized = dep_path.canonicalize().unwrap_or_else(|_| dep_path.clone());
                if !processed.contains(&dep_normalized) {
                    to_process.push(dep_normalized.clone());
                }
                self.module_graph.add_dependency(&normalized_current, &dep_normalized)?;
            }            let mut module_info = super::cache::ModuleInfo::new(normalized_current.clone())?;
            module_info.imports = program.imports.clone();
            module_info.program = Some(program.clone());
            module_info.extract_symbols(&program);
            
            self.module_graph.build_symbol_dependencies(&normalized_current, &program);
            self.module_graph.add_module(module_info);
        }

        Ok(())
    }

    pub fn get_entry_file_id(&self, files: &mut codespan::Files<String>, entry_path: &Path) -> Result<codespan::FileId> {
        let content = std::fs::read_to_string(entry_path)?;
        Ok(files.add(entry_path.to_string_lossy().to_string(), content))
    }

    pub fn get_imported_info(&self) -> Result<(std::collections::HashMap<String, (Vec<crate::ast::Type>, crate::ast::Type)>, Vec<crate::ast::StructDef>, Vec<crate::ast::FfiVariable>)> {
        let mut imported_functions = std::collections::HashMap::new();
        let mut imported_structs = Vec::new();
        let mut imported_ffi_vars = Vec::new();

        for (_, module_info) in &self.module_graph.modules {
            if let Some(program) = &module_info.program {
                for function in &program.functions {
                    if matches!(function.visibility, crate::ast::Visibility::Public) {
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
    }    pub fn compile_all_modules(
        &mut self,
        files: &mut codespan::Files<String>,
        verbose: bool,
    ) -> Result<Vec<PathBuf>> {
        let mut compiled_modules = Vec::new();
        
        let sorted_modules = self.module_graph.topological_sort()?;
        
        let mut changed_modules = std::collections::HashSet::new();
        for module_path in &sorted_modules {
            if module_path.exists() {
                let changed = self.check_module_for_changes(module_path)?;
                if changed {
                    changed_modules.insert(module_path.clone());
                    if verbose {
                        println!("Detected change in: {}", module_path.display());
                    }
                }
            }
        }
        
        self.module_graph.optimize_cycle_dependencies()?;
        
        for module_path in sorted_modules {
            let needs_compilation = self.needs_compilation(&module_path, &changed_modules)?;
            
            if needs_compilation {
                if verbose {
                    let reason = if changed_modules.contains(&module_path) {
                        "file changed"
                    } else if let Some(module_info) = self.module_graph.modules.get(&module_path) {
                        if !module_info.symbol_changes.is_empty() {
                            "symbol interface changed"
                        } else {
                            "dependency changed"
                        }
                    } else {
                        "dependency changed"
                    };
                    println!("Compiling {}: {}", module_path.display(), reason);
                }
                
                self.compile_module(files, &module_path)?;
                compiled_modules.push(module_path.clone());
                
                changed_modules.insert(module_path);
            } else if verbose {
                println!("Skipping {}: up to date", module_path.display());
            }
        }
        
        if verbose && compiled_modules.is_empty() {
            println!("All modules are up to date");
        }
        
        Ok(compiled_modules)
    }
      fn needs_compilation(&self, module_path: &Path, changed_modules: &std::collections::HashSet<PathBuf>) -> Result<bool> {
        if !module_path.exists() {
            return Ok(false);
        }
        
        if changed_modules.contains(module_path) {
            return Ok(true);
        }
        
        if !self.build_cache.is_cache_valid(module_path)? {
            return Ok(true);
        }
        
        if let Some(module_info) = self.module_graph.modules.get(module_path) {
            if !module_info.symbol_changes.is_empty() {
                return Ok(true);
            }
            
            for symbol_name in module_info.exported_symbols.keys() {
                let symbol_id = format!("{}::{}", module_path.display(), symbol_name);
                if let Some(symbol_info) = module_info.exported_symbols.get(symbol_name) {
                    if !self.build_cache.is_symbol_cache_valid(&symbol_id, &symbol_info.signature_hash) {
                        return Ok(true);
                    }
                }
            }
        }

        let dependencies = self.module_graph.get_dependencies(module_path);
        for dep in dependencies {
            if changed_modules.contains(&dep) {
                return Ok(true);
            }
        }
        
        Ok(false)
    }
      fn compile_module(&mut self, _files: &mut codespan::Files<String>, module_path: &Path) -> Result<()> {
        if let Some(module_info) = self.module_graph.modules.get_mut(module_path) {
            module_info.update_if_changed()?;
            
            module_info.is_dirty = false;
            module_info.symbol_changes.clear();
              if let Some(_program) = &module_info.program {
                for symbol_name in module_info.exported_symbols.keys() {
                    let symbol_id = format!("{}::{}", module_path.display(), symbol_name);
                    if let Some(symbol_info) = module_info.exported_symbols.get(symbol_name) {
                        let deps = self.build_cache.get_symbol_dependencies(&symbol_id);
                        self.build_cache.cache_symbol(&symbol_id, symbol_info.signature_hash.clone(), deps)?;
                    }
                }
            }
            
            let dependencies = self.module_graph.get_dependencies(module_path);
            self.build_cache.cache_module(module_path, dependencies, Vec::new())?;
        }
        
        Ok(())
    }    pub fn create_merged_program(&self, entry_point: &Path) -> Result<crate::ast::Program> {
        let content = std::fs::read_to_string(entry_point)?;
        let mut files = codespan::Files::<String>::new();
        let file_id = files.add(entry_point.to_string_lossy().to_string(), content);
        
        let lexer = crate::lexer::Lexer::new(&files, file_id);
        let mut parser = crate::parser::Parser::new(lexer);
        let mut program = parser.parse().map_err(|e| {
            crate::helpers::print_parse_error(&files, file_id, &e, entry_point);
            let line_col = crate::helpers::extract_line_col_from_error(&files, file_id, &e);
            let clean_path = entry_point.to_string_lossy().replace("\\\\?\\", "");
            anyhow::anyhow!("{}:{}: {}", clean_path, line_col, e.message)
        })?;


        let is_prelude_module = entry_point.to_string_lossy().ends_with("prelude.ve");
        let has_prelude_import = program.imports.iter().any(|import| {
            match import {
                crate::ast::ImportDeclaration::ImportAll { module_path, .. } => module_path == "std/prelude",
                crate::ast::ImportDeclaration::ExportImportAll { module_path, .. } => module_path == "std/prelude",
                _ => false,
            }
        });

        if !is_prelude_module && !has_prelude_import {
            let prelude_import = crate::ast::ImportDeclaration::ImportAll {
                module_path: "std/prelude".to_string(),
                module_type: crate::ast::ModuleType::Standard,
                alias: None,
            };
            program.imports.insert(0, prelude_import);
        }

        for (_, module_info) in &self.module_graph.modules {
            if let Some(module_program) = &module_info.program {
                for function in &module_program.functions {
                    if matches!(function.visibility, crate::ast::Visibility::Public) {
                        program.functions.push(function.clone());
                    }
                }
                
                for struct_def in &module_program.structs {
                    if matches!(struct_def.visibility, crate::ast::Visibility::Public) {
                        program.structs.push(struct_def.clone());
                    }
                }
                

                for ffi_func in &module_program.ffi_functions {
                    program.ffi_functions.push(ffi_func.clone());
                }

                
                for ffi_var in &module_program.ffi_variables {
                    program.ffi_variables.push(ffi_var.clone());
                }
            }
        }

        Ok(program)
    }
}
