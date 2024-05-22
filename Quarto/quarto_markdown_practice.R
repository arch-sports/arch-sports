require(gt)
getHdata(support)
# Define a data frame that forms the table rows and columns
set.seed(1)
d <- data.frame(
  Item              = c(runif(3), NA),
  chi               = rchisq(4, 3),
  '$$X_2$$'         = rnorm(4),
  Markdown          = c('* part 1\n* part 2\n* part 3', '', '', '**xxx**'),
  Y                 = c('$$\\alpha_{3}^{4}$$', rep('', 3)),
  check.names = FALSE)   # allows illegal R column names

output <- gt(d)                                                |>
  tab_header(title=md('**Main Title $\\beta_3$ Using `gt`**'),
             subtitle='Some Subtitle')               |>
  tab_options(table.width=pct(65))                   |>
  tab_spanner('Numeric Variables', columns=1:3)      |>
  tab_spanner('Non-Numeric Variables',
              columns=c(Markdown, Y))                |>
  tab_row_group(md('**After** Intervention'),  rows=3:4) |>
  tab_row_group('Before Intervention', rows=1:2)     |>
  tab_options(row_group.font.weight='bold',
              row_group.background.color='lightgray')|>
  sub_missing(missing_text='')                       |>
  fmt_number(columns=c(Item, '$$X_2$$'), decimals=2) |>
  cols_label(Y   ~ html('Velocity<br>of Thing'),
             chi ~ md('$$\\chi^2_{3}$$'))            |>
  cols_width(Markdown ~ px(160))                     |>
  cols_align(align='center', columns=Y)              |>
  fmt_markdown(columns=Markdown, rows=1)             |>
  tab_style(style=cell_text(size='small'),
            locations=cells_body(columns=Markdown))  |>
  tab_style(style=cell_text(color='blue', align='right'),
            locations=cells_column_labels(columns='$$X_2$$')) |>
  tab_source_note(md('_Note_: There is a bug in `tab_row_group` in `gt` version 0.9.0 causing the row group labels to appear in the reverse order in which they were named.  This is why the `tab_row_group` were reversed in the code.  The problem is reported [here](https://github.com/rstudio/gt/issues/717).'))    |>
  tab_footnote(md('Carefully calculated based on _bad_ assumptions'),
               locations=cells_body(columns=Item,
                                    rows=Item==min(Item, na.rm=TRUE)))

print(output)

options(prType='html')
des <- describe(support)
print(des, 'continuous')