return {
  { -- Main File Manager
    'mikavilpas/yazi.nvim',
    event = 'VeryLazy',
    lazy = 'false',
    hidden = 'true',
    keys = {
      -- 👇 in this section, choose your own keymappings!
      {
        -- space + n to open
        -- '<leader>o',
        '<leader>op',
        mode = { 'n', 'v' },
        '<cmd>Yazi<cr>',
        desc = 'Open yazi at the current file',
      },
    },
    ---@type YaziConfig
    opts = {
      -- if you want to open yazi instead of netrw, see below for more info
      open_for_directories = true,
      yazi_floating_window_border = 'none',
      floating_window_scaling_factor = 1,
      -- yazi_floating_window_winblend = 20,
    },
  },
  {
    'stevearc/oil.nvim',
    opts = {},
    dependencies = { { 'echasnovski/mini.icons', opts = {} } },
    config = function()
      require('oil').setup {
        default_file_explorer = true,
        view_options = {
          show_hidden = true,
        },
        keymaps = {
          vim.keymap.set('n', '<leader>o/', ':Oil<CR>'),
        },
      }
    end,
  },
}
