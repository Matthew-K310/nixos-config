return {
  'williamboman/mason-lspconfig.nvim',
  config = function()
    require('mason-lspconfig').setup {
      ensure_installed = {
        'bashls',
        'lua_ls',
        'rust_analyzer',
        'gopls',
        'templ',
        'html',
        'cssls',
        'emmet_language_server',
        'htmx',
        -- 'tailwindcss',
        'ts_ls',
        'pylsp',
        'clangd',
        'prismals',
        'yamlls',
        'jsonls',
        'eslint',
        'marksman',
        'sqlls',
        -- 'sqls',
        'wgsl_analyzer',
        'texlab',
        'templ',
        'intelephense',
        'cmake',
        'rust_analyzer',
        'astro',
        'texlab',
        'nim_langserver',
        'zls',
      },
    }
    require('mason').setup()

    local mason_registry = require 'mason-registry'

    local formatters = {
      'prettier',
      'stylua',
      'eslint_d',
      'gofumpt',
      'golines',
      'goimports-reviser',
      'black',
      'shfmt',
      'sleek',
    }

    -- Ensure formatters are installed
    for _, tool in ipairs(formatters) do
      if not mason_registry.is_installed(tool) then
        mason_registry.get_package(tool):install()
      end
    end
  end,
}
