-- Lua filter to handle format-specific conversions for both PDF and HTML output
-- Handles:
--   - Right-aligned images (via .right-align class)
--   - Page breaks (via \newpage LaTeX command)
--   - Title page font sizes and styles (via custom classes)
--   - Format-specific image variants (PDF vs SVG/PNG)
--   - Vertical fill (vfill for LaTeX)
--   - Unnumbered sections (via .unnumbered class)
--   - Unicode checkmarks (convert to \checkmark for LaTeX)

function Str(elem)
  -- Replace Unicode checkmark (✓) with LaTeX \checkmark command
  if FORMAT:match 'latex' then
    if elem.text == '✓' then
      return pandoc.RawInline('latex', '$\\checkmark$')
    end
  end
  return elem
end

function Image(elem)
  -- Handle format-specific image variants
  if elem.classes:includes('format-variant') then
    local base_path = elem.src
    if FORMAT:match 'latex' then
      -- Use PDF version for LaTeX
      elem.src = base_path .. '.pdf'
    elseif FORMAT:match 'html' then
      -- Try SVG first, fallback to PNG for HTML
      elem.src = base_path .. '.svg'
    end
  end
  return elem
end

function Para(elem)
  -- Check if paragraph contains a single image with 'right-align' class
  if #elem.content == 1 and elem.content[1].t == 'Image' then
    local img = elem.content[1]
    if img.classes:includes('right-align') then
      if FORMAT:match 'latex' then
        -- For LaTeX/PDF output
        return {
          pandoc.RawBlock('latex', '\\begin{flushright}'),
          elem,
          pandoc.RawBlock('latex', '\\end{flushright}')
        }
      elseif FORMAT:match 'html' then
        -- For HTML output
        local div = pandoc.Div({elem})
        div.attributes['style'] = 'text-align: right;'
        return div
      end
    end
  end
  return elem
end

function RawBlock(elem)
  -- Handle \newpage commands
  if elem.text == '\\newpage' then
    if FORMAT:match 'latex' then
      -- Keep as-is for LaTeX/PDF
      return elem
    elseif FORMAT:match 'html' then
      -- Convert to page break for HTML
      return pandoc.RawBlock('html', '<div style="page-break-after: always;"></div>')
    end
  end
  return elem
end

function HorizontalRule(elem)
  -- Handle horizontal rules
  if FORMAT:match 'latex' then
    -- Check for pagebreak class first
    if elem.classes and elem.classes:includes('pagebreak') then
      return pandoc.RawBlock('latex', '\\newpage')
    else
      -- Use full-width horizontal rule
      return pandoc.RawBlock('latex', '\\noindent\\rule{\\textwidth}{0.5pt}')
    end
  elseif FORMAT:match 'html' then
    if elem.classes and elem.classes:includes('pagebreak') then
      return pandoc.RawBlock('html', '<div style="page-break-after: always;"></div>')
    end
  end
  return elem
end

function Span(elem)
  -- Handle custom title font classes
  if elem.classes:includes('title-main') then
    if FORMAT:match 'latex' then
      local result = {pandoc.RawInline('latex', '{\\fontsize{32}{38}\\selectfont\\sffamily\\bfseries ')}
      for _, v in ipairs(elem.content) do
        table.insert(result, v)
      end
      table.insert(result, pandoc.RawInline('latex', '}'))
      return result
    elseif FORMAT:match 'html' then
      elem.attributes['style'] = 'font-size: 32px; font-weight: bold; font-family: sans-serif;'
    end
  elseif elem.classes:includes('title-sub') then
    if FORMAT:match 'latex' then
      local result = {pandoc.RawInline('latex', '{\\fontsize{24}{29}\\selectfont\\sffamily\\bfseries ')}
      for _, v in ipairs(elem.content) do
        table.insert(result, v)
      end
      table.insert(result, pandoc.RawInline('latex', '}'))
      return result
    elseif FORMAT:match 'html' then
      elem.attributes['style'] = 'font-size: 24px; font-weight: bold; font-family: sans-serif;'
    end
  elseif elem.classes:includes('title-section') then
    if FORMAT:match 'latex' then
      local result = {pandoc.RawInline('latex', '{\\fontsize{20}{24}\\selectfont\\sffamily\\bfseries ')}
      for _, v in ipairs(elem.content) do
        table.insert(result, v)
      end
      table.insert(result, pandoc.RawInline('latex', '}'))
      return result
    elseif FORMAT:match 'html' then
      elem.attributes['style'] = 'font-size: 20px; font-weight: bold; font-family: sans-serif;'
    end
  elseif elem.classes:includes('title-normal') then
    if FORMAT:match 'latex' then
      local result = {pandoc.RawInline('latex', '{\\sffamily ')}
      for _, v in ipairs(elem.content) do
        table.insert(result, v)
      end
      table.insert(result, pandoc.RawInline('latex', '}'))
      return result
    elseif FORMAT:match 'html' then
      elem.attributes['style'] = 'font-family: sans-serif;'
    end
  elseif elem.classes:includes('title-meta') then
    if FORMAT:match 'latex' then
      local result = {pandoc.RawInline('latex', '{\\sffamily\\itshape ')}
      for _, v in ipairs(elem.content) do
        table.insert(result, v)
      end
      table.insert(result, pandoc.RawInline('latex', '}'))
      return result
    elseif FORMAT:match 'html' then
      elem.attributes['style'] = 'font-family: sans-serif; font-style: italic;'
    end
  end
  return elem
end

function Header(elem)
  -- Handle unnumbered sections
  if elem.classes:includes('unnumbered') then
    -- Pandoc's built-in support: just set the unnumbered attribute
    elem.attributes['unnumbered'] = 'true'
  end
  return elem
end

function CodeBlock(elem)
  -- Add styling to code blocks for HTML
  -- For LaTeX, styling is handled in header-includes.tex
  if FORMAT:match 'html' then
    -- Add a class that can be styled with CSS
    if not elem.classes:includes('code-styled') then
      elem.classes:insert('code-styled')
    end
  end
  return elem
end

function Div(elem)
  -- Handle legal table
  if elem.classes:includes('legal-table') then
    -- Find the table inside the div
    for i, block in ipairs(elem.content) do
      if block.t == 'Table' then
        if FORMAT:match 'latex' then
          -- Extract image and text from table cells
          local img_src = nil
          local img_width = nil
          local text_content = nil

          -- Get first row from the table body
          if block.bodies and #block.bodies > 0 then
            local body = block.bodies[1]
            if body.body and #body.body > 0 then
              local row = body.body[1]
              -- Row is a pandoc.Row object with a 'cells' field
              if row.cells and #row.cells > 0 then
                -- First cell (image)
                local cell1 = row.cells[1]
                if cell1.contents and #cell1.contents > 0 then
                  for _, c in ipairs(cell1.contents) do
                    if c.t == 'Plain' or c.t == 'Para' then
                      for _, inline in ipairs(c.content) do
                        if inline.t == 'Image' then
                          img_src = inline.src
                          img_width = inline.attributes.width or '2cm'
                        end
                      end
                    end
                  end
                end
                -- Second cell (text)
                if #row.cells > 1 then
                  local cell2 = row.cells[2]
                  if cell2.contents and #cell2.contents > 0 then
                    text_content = cell2.contents
                  end
                end
              end
            end
          end

          if img_src and text_content then
            -- Adjust image path for LaTeX (relative paths need ../../../../common/ prefix)
            local latex_img_src = img_src
            if not img_src:match('^/') and not img_src:match('^%.%.') then
              -- Relative path, needs adjustment
              latex_img_src = '../../../../common/' .. img_src
            end

            -- Build LaTeX table with custom column widths
            local latex = '\\begin{tabular}{@{}p{2.5cm}p{12cm}@{}}\n'
            latex = latex .. '\\includegraphics[width=' .. img_width .. ']{' .. latex_img_src .. '} & '

            -- Convert text content to LaTeX
            local text_blocks = {}
            for _, block in ipairs(text_content) do
              table.insert(text_blocks, block)
            end
            local text_latex = pandoc.write(pandoc.Pandoc(text_blocks), 'latex')
            -- Remove extra newlines and trim
            text_latex = text_latex:gsub('\n\n', ' '):gsub('^\n', ''):gsub('\n$', '')

            latex = latex .. text_latex .. ' \\\\\n'
            latex = latex .. '\\end{tabular}'

            return pandoc.RawBlock('latex', latex)
          end
        elseif FORMAT:match 'html' then
          -- For HTML, add styling to the table
          for i, block in ipairs(elem.content) do
            if block.t == 'Table' then
              -- Add attributes to style the table
              elem.attributes['style'] = 'border-top: none; border-bottom: none;'
            end
          end
          return elem
        end
      end
    end
  end

  -- Handle tip boxes
  if elem.classes:includes('tip') then
    if FORMAT:match 'latex' then
      local result = {pandoc.RawBlock('latex', '\\begin{tipbox}')}
      for _, v in ipairs(elem.content) do
        table.insert(result, v)
      end
      table.insert(result, pandoc.RawBlock('latex', '\\end{tipbox}'))
      return result
    elseif FORMAT:match 'html' then
      -- For HTML, style as a blue-bordered box with "Tip" title
      local tip_header = pandoc.Div(
        {pandoc.Para({pandoc.Strong({pandoc.Str("Tip")})})},
        {style = 'background-color: #60aad2; color: white; padding: 8px 12px; font-weight: bold; margin: 0;'}
      )
      local tip_content = pandoc.Div(
        elem.content,
        {style = 'background-color: white; border: 2px solid #60aad2; border-top: none; padding: 12px; margin: 0;'}
      )
      local tip_container = pandoc.Div(
        {tip_header, tip_content},
        {style = 'margin: 1em 0; border-radius: 4px; overflow: hidden;'}
      )
      return tip_container
    end
  end

  -- Handle vertical fill (push content to bottom)
  if elem.classes:includes('vfill') then
    if FORMAT:match 'latex' then
      return pandoc.RawBlock('latex', '\\vfill')
    elseif FORMAT:match 'html' then
      -- For HTML, use flexbox spacer
      elem.attributes['style'] = 'flex-grow: 1;'
    end
  end
  -- Handle centered content
  if elem.classes:includes('center') then
    if FORMAT:match 'latex' then
      local result = {pandoc.RawBlock('latex', '\\begin{center}')}
      for _, v in ipairs(elem.content) do
        table.insert(result, v)
      end
      table.insert(result, pandoc.RawBlock('latex', '\\end{center}'))
      return result
    elseif FORMAT:match 'html' then
      elem.attributes['style'] = 'text-align: center;'
    end
  end
  -- Style code block containers in HTML
  if FORMAT:match 'html' and elem.classes then
    for i = 1, #elem.classes do
      if elem.classes[i] == 'sourceCode' then
        elem.attributes['style'] = 'background-color: #f5f5f5; border: 1px solid #cccccc; padding: 10px; border-radius: 4px; margin: 1em 0;'
        break
      end
    end
  end
  return elem
end
