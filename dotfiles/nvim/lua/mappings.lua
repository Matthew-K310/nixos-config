-- local autocmd = vim.api.nvim_create_autocmd

vim.api.nvim_set_keymap('i', 'jj', '<Esc>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>e', ':ene <BAR> startinsert <CR>', { noremap = true, silent = true })

vim.api.nvim_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', { noremap = true, silent = true })

vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- autocomplete in normal text
vim.keymap.set('i', '<C-f>', '<C-x><C-f>', { noremap = true, silent = true })

-- fzf-lua mappings
vim.keymap.set('n', '<leader>.', ":lua require('fzf-lua').files({ cwd = '..' })<CR>") --search above
-- find files in project
vim.keymap.set('n', '<leader> ', ":lua require('fzf-lua').files()<CR>") --search cwd
vim.keymap.set('n', '<leader>pf', ":lua require('fzf-lua').files()<CR>") --search cwd
vim.keymap.set('n', '<leader>ns', ":lua require('fzf-lua').files({ cwd = '~/Notes/obsidian-vault' })<CR>") --search notes
vim.keymap.set('n', '<leader>fr', ":lua require('fzf-lua').oldfiles()<CR>") --last search
vim.keymap.set('n', '<leader>sg', ":lua require('fzf-lua').grep()<CR>") --grep
vim.keymap.set('n', '<leader>sG', ":lua require('fzf-lua').grep_cword()<CR>") --grep word under cursor
vim.keymap.set('n', '<leader>s/', ":lua require('fzf-lua').grep_curbuf()<CR>") -- grep in current file
vim.keymap.set('n', '<leader>ds', ":lua require('fzf-lua').lsp_document_symbols()<CR>")
vim.keymap.set('n', '<leader>ws', ":lua require('fzf-lua').lsp_workspace_symbols()<CR>")
vim.keymap.set('n', '<leader>df', ":lua require('fzf-lua').lsp_definitions()<CR>")
vim.keymap.set('n', '<leader>dr', ":lua require('fzf-lua').lsp_references()<CR>")
vim.keymap.set('n', '<leader>ht', ":lua require('fzf-lua').colorschemes()<CR>")
