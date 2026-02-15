-- ~/.config/nvim/lua/plugins/fzf.lua
-- Corrected content: Directly return the plugin specification table
return {
  'ibhagwan/fzf-lua',
  dependencies = { 'echasnovski/mini.icons' },
  event = 'VeryLazy',
  -- Optional: Add a config function for fzf-lua itself if you have global setup
  -- config = function()
  --   require('fzf-lua').setup {
  --     -- your fzf-lua global settings here
  --   }
  -- end,
}
