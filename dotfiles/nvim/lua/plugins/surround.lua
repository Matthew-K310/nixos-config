return {
  'echasnovski/mini.surround',
  opts = {
    custom_surroundings = nil,
    highlight_duration = 500,
    mappings = {
      add = 'ma', -- Add surrounding in Normal and Visual modes
      delete = 'md', -- Delete surrounding
      find = 'mf', -- Find surrounding (to the right)
      find_left = 'mF', -- Find surrounding (to the left)
      highlight = 'mh', -- Highlight surrounding
      replace = 'mr', -- Replace surrounding
      update_n_lines = 'mn', -- Update `n_lines`
      suffix_last = 'l', -- Suffix to search with "prev" method
      suffix_next = 'n', -- Suffix to search with "next" method
    },
    n_lines = 20,
    respect_selection_type = false,
    search_method = 'cover',
    silent = false,
  },
}
