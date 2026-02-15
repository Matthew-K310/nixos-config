return { -- Highlight, edit, and navigate code
  'nvim-treesitter/nvim-treesitter',
  lazy = false,
  dependencies = {
    'vrischmann/tree-sitter-templ',
  },
  build = ':TSUpdate',
  main = 'nvim-treesitter.configs', -- Sets main module to use for opts
  -- [[ Configure Treesitter ]] See `:help nvim-treesitter`
  ignore_install = {},
  ensure_installed = {
    'purescript',
    'nix',
    'nim',
    'vimdoc',
    'go',
    'rust',
    'c',
    'lua',
    'python',
    'html',
    'css',
    'javascript',
    'typescript',
    'prisma',
    'haskell',
    'zig',
    'gleam',
    'wgsl',
    'php',
    'nim',
    'sql',
    'markdown',
    'latex',
    'gdscript',
    'gdshader',
    'templ',
  },
  highlight = { enable = true },
  indent = { enable = true },
  modules = {},
  sync_install = true,
  auto_install = true,
  -- There are additional nvim-treesitter modules that you can use to interact
  -- with nvim-treesitter. You should go explore a few and see what interests you:
  --
  --    - Incremental selection: Included, see `:help nvim-treesitter-incremental-selection-mod`
  --    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
  --    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
}
