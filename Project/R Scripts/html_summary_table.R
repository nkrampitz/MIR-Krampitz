### Summary of Benthic Surveys for HTML formatting
## Want the table to be different for html formatting and not just a picture
## Due to the nature of the combined headers (M:IR over two columns), formatting of the table is very complicated
## Thus this long, descriptive, script was created and seperated for cleaner output in the final quarto

library(gt)
library(dplyr)

library(gt)
library(dplyr)

df <- tribble(
  ~Year,   ~`M:IR Demo`, ~`M:IR BCA`, ~`NCRMP Demo`, ~`NCRMP BCA`, ~`DRM Demo`,
  "2022",  90,           89,          95,            92,           218,
  "2024",  100,          100,         129,           129,          313
)

html_table <- df %>%
  gt(rowname_col = "Year") %>%
  tab_spanner(label = "M:IR", columns = c(`M:IR Demo`, `M:IR BCA`)) %>%
  tab_spanner(label = "NCRMP", columns = c(`NCRMP Demo`, `NCRMP BCA`)) %>%
  tab_spanner(label = "DRM", columns = c(`DRM Demo`)) %>%
  grand_summary_rows(
    columns = c(`M:IR Demo`, `M:IR BCA`, `NCRMP Demo`, `NCRMP BCA`, `DRM Demo`),
    fns = list(
      Total = ~sum(., na.rm = TRUE)
    )
  ) %>%
 #Demo/BCA labels
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(
      columns = c(`M:IR Demo`, `M:IR BCA`, `NCRMP Demo`, `NCRMP BCA`, `DRM Demo`)
    )) %>%
  ## Total Label
   tab_style(
    style = list(
      cell_text(weight = "bold", style = "italic", size = px(21), align = "center")),
    locations = cells_stub_grand_summary()
  ) %>%

  # Summary Numbers
   tab_style(
    style = list(
      cell_fill(color = "#f0f0f0"),
      cell_text(weight = "bold", align = "center", style = "italic", size = px(23))
    ),
    locations = cells_grand_summary()
  ) %>%

  # Center all numbers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%

  # Bold spanner labels (M:IR, NCRMP, DRM)
  tab_style(
    style = cell_text(weight = "bold", size = px(20)),
    locations = cells_column_spanners()
  ) %>%
  cols_label(
    `M:IR Demo` = md("*Demo*"),
    `M:IR BCA` = md("*BCA*"),
    `NCRMP Demo` = md("*Demo*"),
    `NCRMP BCA` = md("*BCA*"),
    `DRM Demo` = md("*Demo*")
  ) %>%
  # Light grey background ONLY for the Demo/BCA column labels — NOT the stub header
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_column_labels(columns = c(`M:IR Demo`, `M:IR BCA`, `NCRMP Demo`, `NCRMP BCA`, `DRM Demo`))
  )  %>%
  tab_options(
    table.align = "center",
    table.width = px(600),
    table.font.size = px(18),            # Bigger font for whole table
    data_row.padding = px(12),            # Bigger vertical padding for rows
    grand_summary_row.padding = px(12),
    summary_row.padding = px(62),
    table.border.top.width = px(0),       # Remove outside border
    table.border.bottom.width = px(0),
    table.border.left.width = px(0),
    table.border.right.width = px(0),
    table.background.color = "white"     # Ensure no grey background on table edges
  )

print(html_table)

