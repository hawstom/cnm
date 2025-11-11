# HawsEDC Developer Tools

This directory contains developer-only tools for HawsEDC/CNM development.

## Scripts

**Location:** `scripts/`  
**Contents:** AutoLISP and PowerShell development tools

### Performance Benchmarking
```lisp
;; Load and test
(load "devtools/scripts/performance-benchmark.lsp")
(c:benchmark-config)  ; Phase 3: 40ms config bottleneck
```

**Key Phase 3 Finding:** 40ms bottleneck in config system due to `hcnm-proj` file validation. 97.5% improvement possible via session caching.

### Build Automation
- `compile.lsp` - AutoLISP compilation system
- `*.ps1` - PowerShell scripts for code transformation

## Documentation

**Location:** `docs/`  
**Contents:** Architecture standards, performance analysis, guides

## Files Structure

```
devtools/
├── scripts/              # AutoLISP and PowerShell development tools
│   ├── performance-benchmark.lsp     # Performance benchmarking system
│   ├── compile.lsp       # AutoLISP compilation system
│   ├── *.ps1             # PowerShell transformation scripts
│   └── README-performance.md  # Benchmarking documentation
├── docs/                 # Development documentation
└── README.md             # This file
```

## Design Philosophy

**Minimal:** Each subsystem should be 2-3 files maximum  
**Self-contained:** No complex dependencies  
**Focused:** Solve specific development needs  
**Clean:** Avoid over-engineering
