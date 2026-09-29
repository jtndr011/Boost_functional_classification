library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(purrr)
library(tibble)

# ─────────────────────────────────────────────────────────
# Module rules
# ─────────────────────────────────────────────────────────

module_rules <- tribble(
  ~priority, ~Functional_module, ~pattern,
  
  1, "Transposable element-associated",
  "retrovirus-related|transposon|pol polyprotein",
  
  2, "Sugar transport / susceptibility-associated nutrient transport",
  "\\bsweet\\b|bidirectional sugar transporter",
  
  3, "JA / oxylipin defense signaling",
  "oxophytodienoate|\\bopda\\b|lipoxygenase|allene oxide synthase|jasmonate|jasmonic",
  
  3.5, "Protein turnover / proteostasis",
  "ftshi|atp-dependent zinc metalloprotease",
  
  4, "Membrane signaling / receptor-associated defense",
  "tyrosine-sulfated glycopeptide receptor|feronia|herk|ralf|remorin|two pore potassium|potassium channel|pto-interacting",
  
  5, "Pathogen perception / immune receptors",
  "g-type lectin s-receptor|lrr receptor-like|leucine-rich repeat receptor-like|putative leucine-rich repeat receptor-like|receptor-like protein 6|receptor-like serine/threonine-protein kinase|sd1-8|probable serine/threonine-protein kinase ddb_g0276461|raf-like serine/threonine-protein kinase|praf|enhanced disease resistance 4|\\bedr4\\b|\\blr10\\b|\\bxa21\\b|\\beix2\\b|\\bripk\\b|\\bhsl1\\b|\\bpbl\\b|\\bpbl19\\b|\\bbsk1\\b|\\bbski\\b|\\bndr1\\b|\\bhin1\\b|\\brga\\b|\\brpp13\\b|\\bpik\\b|\\bpiks\\b|disease resistance|\\bnlr\\b|nb-arc|cysteine-rich receptor|receptor-like protein kinase|receptor-like kinase|lectin-domain containing receptor|wall-associated receptor|\\bwak\\b|lectin 9|ricin b-like lectin",
  
  6, "Hormone / growth regulation",
  "nced|9-cis-epoxycarotenoid dioxygenase|abscisic acid|\\baba\\b|auxin-responsive|\\biaa\\d+\\b|saur|cytokinin dehydrogenase|gibberellin|ent-kaurenoic acid oxidase|dwarf 53|pin-likes|pin-like|dormancy-associated|early nodulin|cyclin-p4",
  
  7, "Calcium signaling",
  "calcium|calmodulin|\\bcml\\b|calcium-binding|calmodulin-binding|cbl-interacting|cipk|glutamate receptor|cyclic nucleotide-gated ion channel|cngc|annexin|src2|calcium-dependent protein kinase|cdpk",
  
  8, "ROS and oxidative burst",
  "peroxidase|peroxygenase|oxalate oxidase|germin|catalase|polyphenol oxidase|sarcosine oxidase|superoxide dismutase|\\bsod\\b|oxidative stress|formate dehydrogenase|ferredoxin|metallothionein|nad\\(p\\)h|ubiquinone oxidoreductase|alternative nad",
  
  9, "Cell wall reinforcement",
  "polygalacturonase inhibitor|polygalacturonase|pectin|methylesterase|xyloglucan|xylan|arabinosyltransferase|cellulose synthase|cellulose synthase-like|extensin|cell wall|galacturonosyltransferase|endoglucanase|beta-glucosidase|fructofuranosidase|mannosyltransferase|fasciclin|arabinogalactan|xylosidase|xylosyltransferase|networked|wir1|cuticle|cuticular|wax|suberin|burp domain",
  
  10, "Antimicrobial activity / secondary metabolism",
  "3beta-hydroxysteroid|horcolin|hydroxysteroid-dehydrogenase|decarboxylase isoform 1|obtusifoliol 14-alpha demethylase|tricetin|o-trimethyltransferase|noroxomaritidine|norcraugsodine reductase|long chain acyl-coa synthetase|very-long-chain aldehyde decarbonylase|putrescine hydroxycinnamoyltransferase|podophyllotoxin|trimethyltridecatetraene|p450|cytochrome p450|cytochrome b5|2-oxoglutarate-dependent dioxygenase|monooxygenase|flavin-containing monooxygenase|chalcone synthase|phenylalanine ammonia-lyase|strictosidine|zealexin|dimboa|glucosyltransferase|glycosyltransferase|carboxylesterase|patatin|linalool|geraniol|terpene|diterpene|copalyl|pimara|cycloartenol|benzoyltransferase|tryptamine|anthranilate|4-coumarate|coumarate|wheatwin|pathogenesis-related|thaumatin|chitinase|prms|sth-21|bowman-birk|lipid transfer|non-specific lipid-transfer|3-ketoacyl-coa synthase|acyl transferase|gdsl|cyclase-like protein",
  
  11, "SAR / immune transcriptional regulation",
  "wrky|ald1|cbp60|nac|ethylene-responsive transcription factor|\\berf\\b|myb|mybs|bhlh|\\bdof\\b|\\btga\\b|\\btcp\\b|\\bpcf\\b|transcription factor|transcription repressor|zinc finger|ccch domain|fcs-like zinc finger|\\balp1\\b|homeobox|b-box|radialis|\\blhw\\b|\\bdreb\\b|mediator of rna polymerase|light-inducible protein|cprf2",
  
  12, "Chromatin / genome regulation",
  "histone|heterochromatin|chromatin|swi/snf",
  
  13, "Detoxification / stress protection",
  "nuclear fusion defective|hspro1|nematode resistance protein-like|l-2-hydroxyglutarate dehydrogenase|ubiquinol oxidase|succinate-semialdehyde dehydrogenase|gamma-aminobutyrate transaminase|protein detoxification|detoxification [0-9]+|abc transporter|glutathione|\\bgst\\b|thioredoxin|\\bhsp\\b|heat shock|nudix|early responsive to dehydration|\\berd\\b|dehydration|stress-associated|universal stress|\\blea\\b|late embryogenesis abundant|abscisic stress-ripening|salt stress-induced|desiccation protectant|serine acetyltransferase|zinc induced facilitator",
  
  14, "Immune feedback / homeostasis",
  "protein phosphatase 2c|pp2c|protein phosphatase|e3 ubiquitin|u-box|btb/poz|f-box|kelch|ubiquitin|ring protein|nrr|sodium/proton antiporter|polyamine transporter|heavy metal-associated|vacuolar-processing enzyme|phytepsin|aspartic proteinase|aspartyl protease|subtilisin-like protease|senescence-specific cysteine protease|oryzasin",
  
  15, "Hypersensitive response / cell death",
  "hypersensitive-induced|accelerated cell death|\\bacd6\\b|rho gtpase-activating|sex determination protein tasselseed-2",
  
  16, "Protein turnover / proteostasis",
  "aaa-atpase|metalloprotease|ftshi|egy2|protein synthesis inhibitor|neprosin|pro-x carboxypeptidase|protein s40|aminopeptidase|insulin-degrading enzyme|proteinase inhibitor|basic secretory protease",
  
  17, "Translation / ribosome biogenesis",
  "ribosomal|ribosome|large ribosomal subunit|small ribosomal subunit|rack1|fibrillarin|nucleolar|u3 small nucleolar|rrna",
  
  18, "RNA processing / organellar regulation",
  "pentatricopeptide|\\bppr\\b|ribonucleoprotein|polyribonucleotide|endoribonuclease|exoribonuclease|rna pseudouridine|ybey|splicing factor|serine/arginine-rich|glycine-rich rna-binding|yth domain|rna-binding",
  
  19, "DNA repair / genome maintenance",
  "dna-repair|xrcc1|formamidopyrimidine-dna glycosylase|dna glycosylase|atp-dependent dna helicase",
  
  20, "Membrane trafficking / cytoskeleton",
  "microtubule|tubulin|actin|actin-depolymerizing|multiple c2 domain|phosphatidylinositol transfer|pns1|tortifolia|transmembrane protein|ap-1 complex|ala-interacting|bag-associated gram|filament-like|ist1|longifolia|rhomboid|rop guanine nucleotide exchange factor|tlc domain|spinster",
  
  21, "Primary metabolism / nutrient transport",
  "butanoate--coa ligase|aae1|cell number regulator|heparanase-like|lecithin-cholesterol acyltransferase-like|beta-1,3-galactosyltransferase|alpha-glucan water dikinase|phosphate transporter|inorganic phosphate transporter|low affinity inorganic phosphate transporter|sucrose synthase|sucrose|fructosyltransferase|fructokinase|beta-amylase|ump synthase|uridine|ctp synthase|gmp synthase|7-methyl-gtp|amino-acid permease|nrt1|ptr family|phosphate|carbonic anhydrase|aquaporin|\\bpip\\b|\\btip\\b|\\bnip\\b|asparagine synthetase|aconitate hydratase|succinate dehydrogenase|potassium transporter|lysine histidine transporter|oligopeptide transporter|tonoplast dicarboxylate transporter|vacuolar iron transporter|metal-nicotianamine transporter|magnesium transporter|udp-glucose 4-epimerase|adenosylhomocysteinase|cytidine deaminase|6-phosphogluconolactonase|fatty-acid desaturase|glycerophosphodiester phosphodiesterase|anaerobic nitrite reductase|phytol kinase",
  
  22, "Chloroplast redox / photosynthetic stress",
  "chloroplastic|chloroplast|thylakoid|photosystem|rubisco|plastid|tic 32|ccs1|cytochrome c biogenesis|protein cia1|chaperonin|cpn60|translocase of chloroplast"
)

# ─────────────────────────────────────────────────────────
# Helper functions
# ─────────────────────────────────────────────────────────

classify_one <- function(x) {
  
  if (is.na(x) || x == "") {
    return(tibble(
      Functional_module = "Other / unknown",
      matched_modules = NA_character_,
      n_module_hits = 0
    ))
  }
  
  hits <- module_rules %>%
    filter(str_detect(x, regex(pattern, ignore_case = TRUE))) %>%
    arrange(priority)
  
  if (nrow(hits) == 0) {
    return(tibble(
      Functional_module = "Other / unknown",
      matched_modules = NA_character_,
      n_module_hits = 0
    ))
  }
  
  tibble(
    Functional_module = hits$Functional_module[1],
    matched_modules = paste(unique(hits$Functional_module), collapse = " | "),
    n_module_hits = n_distinct(hits$Functional_module)
  )
}

add_fig_module <- function(x) {
  case_when(
    x %in% c(
      "Pathogen perception / immune receptors",
      "Membrane signaling / receptor-associated defense"
    ) ~ "Immune perception and signal initiation",
    
    x %in% c(
      "Calcium signaling",
      "ROS and oxidative burst"
    ) ~ "Calcium–ROS immune amplification",
    
    x == "Cell wall reinforcement" ~
      "Cell-wall reinforcement and apoplastic defense",
    
    x %in% c(
      "Antimicrobial activity / secondary metabolism",
      "Detoxification / stress protection",
      "JA / oxylipin defense signaling"
    ) ~ "Antimicrobial metabolism and PR defense",
    
    x %in% c(
      "SAR / immune transcriptional regulation",
      "Chromatin / genome regulation"
    ) ~ "Transcriptional regulation and systemic immunity",
    
    x == "Immune feedback / homeostasis" ~
      "Immune feedback and homeostasis",
    
    x %in% c(
      "Chloroplast redox / photosynthetic stress",
      "Hormone / growth regulation",
      "Hypersensitive response / cell death",
      "Primary metabolism / nutrient transport",
      "Protein turnover / proteostasis",
      "RNA processing / organellar regulation",
      "DNA repair / genome maintenance",
      "Translation / ribosome biogenesis",
      "Sugar transport / susceptibility-associated nutrient transport",
      "Membrane trafficking / cytoskeleton",
      "Transposable element-associated"
    ) ~ "Metabolic and cellular reprogramming",
    
    TRUE ~ "Other / unknown"
  )
}

# ─────────────────────────────────────────────────────────
# Main pipeline
# ─────────────────────────────────────────────────────────

run_functional_module_pipeline <- function(infile, outdir, prefix) {
  
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  df <- read.csv(
    infile,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  df <- df[, !is.na(colnames(df)) & colnames(df) != ""]
  
  df <- df %>%
    mutate(
      `Functional annotation` = str_replace_all(`Functional annotation`, "%2C", ","),
      annotation_lower = str_to_lower(`Functional annotation`)
    ) %>%
    bind_cols(map_dfr(.$annotation_lower, classify_one)) %>%
    mutate(
      Fig_module = add_fig_module(Functional_module)
    )
  
  ambiguity_check <- df %>%
    filter(n_module_hits > 1) %>%
    dplyr::select(
      `Functional annotation`,
      Functional_module,
      matched_modules,
      n_module_hits
    ) %>%
    arrange(desc(n_module_hits))
  
  log2fc_cols <- grep("_log2FC$", colnames(df), value = TRUE)
  padj_cols <- grep("_padj$", colnames(df), value = TRUE)
  
  long_df <- df %>%
    pivot_longer(
      cols = all_of(c(log2fc_cols, padj_cols)),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    mutate(
      Comparison = str_remove(Metric, "_log2FC$|_padj$"),
      Stat = case_when(
        str_detect(Metric, "_log2FC$") ~ "log2FC",
        str_detect(Metric, "_padj$") ~ "padj"
      )
    ) %>%
    dplyr::select(-Metric) %>%
    pivot_wider(
      names_from = Stat,
      values_from = Value
    ) %>%
    mutate(
      log2FC = as.numeric(log2FC),
      padj = as.numeric(padj),
      Regulation = case_when(
        !is.na(log2FC) & log2FC > 0 ~ "UP",
        !is.na(log2FC) & log2FC < 0 ~ "DOWN",
        TRUE ~ NA_character_
      ),
      Time = case_when(
        str_detect(Comparison, "24H") ~ "24 HAI",
        str_detect(Comparison, "48H") ~ "48 HAI",
        str_detect(Comparison, "96H") ~ "96 HAI",
        TRUE ~ Comparison
      )
    ) %>%
    filter(!is.na(log2FC), !is.na(Regulation))
  
  module_counts <- long_df %>%
    dplyr::count(Time, Regulation, Functional_module) %>%
    arrange(Time, Regulation, desc(n))
  
  fig_counts <- long_df %>%
    dplyr::count(Time, Regulation, Fig_module) %>%
    arrange(Time, Regulation, desc(n))
  
  fig_counts %>%
    filter(is.na(Fig_module) | is.na(Time) | is.na(Regulation))
  
  module_order <- c(
    "Pathogen perception / immune receptors",
    "Membrane signaling / receptor-associated defense",
    "Calcium signaling",
    "ROS and oxidative burst",
    "SAR / immune transcriptional regulation",
    "Chromatin / genome regulation",
    "Cell wall reinforcement",
    "Antimicrobial activity / secondary metabolism",
    "JA / oxylipin defense signaling",
    "Detoxification / stress protection",
    "Immune feedback / homeostasis",
    "Chloroplast redox / photosynthetic stress",
    "Hormone / growth regulation",
    "Hypersensitive response / cell death",
    "Primary metabolism / nutrient transport",
    "Sugar transport / susceptibility-associated nutrient transport",
    "Membrane trafficking / cytoskeleton",
    "RNA processing / organellar regulation",
    "Translation / ribosome biogenesis",
    "DNA repair / genome maintenance",
    "Protein turnover / proteostasis",
    "Transposable element-associated",
    "Other / unknown"
  )
  
  fig_order <- c(
    "Immune feedback and homeostasis",
    "Metabolic and cellular reprogramming",
    "Antimicrobial metabolism and PR defense",
    "Cell-wall reinforcement and apoplastic defense",
    "Transcriptional regulation and systemic immunity",
    "Calcium–ROS immune amplification",
    "Immune perception and signal initiation",
    "Other / unknown"
  )
  
  module_colors <- c(
    "Immune perception and signal initiation" = "#D73027",
    "Calcium–ROS immune amplification" = "#FC8D59",
    "Transcriptional regulation and systemic immunity" = "#66BD63",
    "Cell-wall reinforcement and apoplastic defense" = "#4575B4",
    "Antimicrobial metabolism and PR defense" = "#1A9850",
    "Metabolic and cellular reprogramming" = "#7F7F7F",
    "Immune feedback and homeostasis" = "#984EA3",
    "Other / unknown" = "#D9D9D9"
  )
  
  module_counts <- module_counts %>%
    mutate(
      Time = factor(Time, levels = c("24 HAI", "48 HAI", "96 HAI")),
      Regulation = factor(Regulation, levels = c("UP", "DOWN")),
      Functional_module = factor(Functional_module, levels = module_order)
    )
  
  fig_counts <- fig_counts %>%
    mutate(
      Time = factor(Time, levels = c("24 HAI", "48 HAI", "96 HAI")),
      Regulation = factor(Regulation, levels = c("UP", "DOWN")),
      Fig_module = factor(Fig_module, levels = fig_order)
    )
  
  # ───────────────────────────────────────────────────────
  # Plot 1: all detailed categories
  # ───────────────────────────────────────────────────────
  
  p_all <- ggplot(
    module_counts,
    aes(x = Time, y = n, fill = Functional_module)
  ) +
    geom_col(color = "black", linewidth = 0.2, width = 0.75) +
    facet_wrap(
      ~ Regulation,
      nrow = 1,
      labeller = as_labeller(c("UP" = "Upregulated", "DOWN" = "Downregulated"))
    ) +
    labs(
      x = "",
      y = "Number of DEGs",
      fill = "Functional module"
    ) +
    theme_classic(base_size = 14) +
    theme(
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      strip.text = element_text(face = "bold"),
      strip.background = element_blank(),
      legend.position = "right"
    )
  
  ggsave(file.path(outdir, "Functional_module_stacked_barplot_all_categories.png"),
         p_all, width = 12, height = 7, dpi = 600)
  
  ggsave(file.path(outdir, "Functional_module_stacked_barplot_all_categories.pdf"),
         p_all, width = 12, height = 7)
  
  # ───────────────────────────────────────────────────────
  # Plot 2: defense-focused detailed categories
  # ───────────────────────────────────────────────────────
  
  defense_modules <- c(
    "Pathogen perception / immune receptors",
    "Membrane signaling / receptor-associated defense",
    "Calcium signaling",
    "ROS and oxidative burst",
    "SAR / immune transcriptional regulation",
    "Chromatin / genome regulation",
    "Cell wall reinforcement",
    "Antimicrobial activity / secondary metabolism",
    "JA / oxylipin defense signaling",
    "Detoxification / stress protection",
    "Immune feedback / homeostasis",
    "Hormone / growth regulation",
    "Hypersensitive response / cell death"
  )
  
  module_counts_defense <- module_counts %>%
    filter(Functional_module %in% defense_modules)
  
  p_defense <- ggplot(
    module_counts_defense,
    aes(x = Time, y = n, fill = Functional_module)
  ) +
    geom_col(color = "black", linewidth = 0.2, width = 0.75) +
    facet_wrap(
      ~ Regulation,
      nrow = 1,
      labeller = as_labeller(c("UP" = "Upregulated", "DOWN" = "Downregulated"))
    ) +
    labs(
      x = "",
      y = "Number of defense-associated DEGs",
      fill = "Functional module"
    ) +
    theme_classic(base_size = 14) +
    theme(
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      strip.text = element_text(face = "bold"),
      strip.background = element_blank(),
      legend.position = "right"
    )
  
  ggsave(file.path(outdir, "Functional_module_stacked_barplot_defense_focused.png"),
         p_defense, width = 12, height = 7, dpi = 600)
  
  ggsave(file.path(outdir, "Functional_module_stacked_barplot_defense_focused.pdf"),
         p_defense, width = 12, height = 7)
  
  # ───────────────────────────────────────────────────────
  # Plot 3: Fig 7-module count plot
  # ───────────────────────────────────────────────────────
  
  p_fig <- ggplot(
    fig_counts,
    aes(x = Time, y = n, fill = Fig_module)
  ) +
    geom_col(color = "black", linewidth = 0.25, width = 0.75) +
    facet_wrap(
      ~ Regulation,
      nrow = 1,
      labeller = as_labeller(c("UP" = "Upregulated", "DOWN" = "Downregulated"))
    ) +
    scale_fill_manual(
      values = module_colors,
      drop = FALSE,
      guide = guide_legend(reverse = TRUE)
    ) +
    labs(
      x = "",
      y = "Number of DEGs",
      fill = "Functional module"
    ) +
    theme_classic(base_size = 15) +
    theme(
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      strip.text = element_text(face = "bold"),
      strip.background = element_blank(),
      legend.position = "right"
    )
  
  ggsave(file.path(outdir, "Fig_7module_stacked_barplot.png"),
         p_fig, width = 11, height = 6.5, dpi = 600)
  
  ggsave(file.path(outdir, "Fig_7module_stacked_barplot.pdf"),
         p_fig, width = 11, height = 6.5)
  
  # ───────────────────────────────────────────────────────
  # Plot 4: Fig percentage WITHOUT n labels
  # ───────────────────────────────────────────────────────
  
  fig_percent_no_n <- fig_counts %>%
    group_by(Time, Regulation) %>%
    mutate(
      Percent = 100 * n / sum(n)
    ) %>%
    ungroup()
  
  p_fig_percent_no_n <- ggplot(
    fig_percent_no_n,
    aes(x = Time, y = Percent, fill = Fig_module)
  ) +
    geom_col(color = "black", linewidth = 0.25, width = 0.75) +
    facet_wrap(
      ~ Regulation,
      nrow = 1,
      labeller = as_labeller(c("UP" = "Upregulated", "DOWN" = "Downregulated"))
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      expand = c(0, 0)
    ) +
    scale_fill_manual(
      values = module_colors,
      drop = FALSE,
      guide = guide_legend(reverse = TRUE)
    ) +
    labs(
      x = "",
      y = "Percentage of DEGs (%)",
      fill = "Functional module"
    ) +
    theme_classic(base_size = 17) +
    theme(
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      strip.text = element_text(face = "bold", size = 16),
      strip.background = element_blank(),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 15),
      legend.text = element_text(size = 12)
    )
  
  ggsave(file.path(outdir, "Fig_functional_module_percentage.png"),
         p_fig_percent_no_n, width = 11, height = 6.5, dpi = 600)
  
  ggsave(file.path(outdir, "Fig_functional_module_percentage.pdf"),
         p_fig_percent_no_n, width = 11, height = 6.5)
  
  # ───────────────────────────────────────────────────────
  # Plot 5: Fig percentage WITH n labels
  # ───────────────────────────────────────────────────────
  
  fig_percent <- fig_counts %>%
    group_by(Time, Regulation) %>%
    mutate(
      Total_n = sum(n),
      Percent = 100 * n / Total_n
    ) %>%
    ungroup()
  
  n_labels <- fig_percent %>%
    distinct(Time, Regulation, Total_n) %>%
    mutate(label = paste0("n = ", Total_n))
  
  p_fig_percent <- ggplot(
    fig_percent,
    aes(x = Time, y = Percent, fill = Fig_module)
  ) +
    geom_col(color = "black", linewidth = 0.25, width = 0.75) +
    geom_text(
      data = n_labels,
      aes(x = Time, y = 103, label = label),
      inherit.aes = FALSE,
      size = 4.5,
      color = "black",
      fontface = "bold"
    ) +
    facet_wrap(
      ~ Regulation,
      nrow = 1,
      labeller = as_labeller(c("UP" = "Upregulated", "DOWN" = "Downregulated"))
    ) +
    scale_y_continuous(
      limits = c(0, 108),
      breaks = seq(0, 100, 20),
      expand = c(0, 0)
    ) +
    scale_fill_manual(
      values = module_colors,
      drop = FALSE,
      guide = guide_legend(reverse = TRUE)
    ) +
    labs(
      x = "",
      y = "Percentage of DEGs (%)",
      fill = "Functional module"
    ) +
    theme_classic(base_size = 17) +
    theme(
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      strip.text = element_text(face = "bold", size = 16),
      strip.background = element_blank(),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 15),
      legend.text = element_text(size = 12)
    )
  
  ggsave(file.path(outdir, "Fig_7module_percentage_with_n.png"),
         p_fig_percent, width = 11, height = 6.5, dpi = 600)
  
  ggsave(file.path(outdir, "Fig_7module_percentage_with_n.pdf"),
         p_fig_percent, width = 11, height = 6.5)
  
  # ───────────────────────────────────────────────────────
  # Plot 6: detailed module heatmap
  # ───────────────────────────────────────────────────────
  
  heat_df <- module_counts %>%
    unite("Time_Regulation", Time, Regulation, sep = "_") %>%
    dplyr::select(Time_Regulation, Functional_module, n) %>%
    pivot_wider(
      names_from = Time_Regulation,
      values_from = n,
      values_fill = 0
    )
  
  write.csv(
    heat_df,
    file.path(outdir, "Functional_module_count_matrix_for_heatmap.csv"),
    row.names = FALSE
  )
  
  heat_long <- module_counts %>%
    mutate(Time_Regulation = paste(Time, Regulation, sep = "_"))
  
  p_heatmap <- ggplot(
    heat_long,
    aes(x = Time_Regulation, y = Functional_module, fill = n)
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = n), size = 3) +
    labs(
      x = "",
      y = "",
      fill = "DEG count"
    ) +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      axis.text.y = element_text(color = "black"),
      legend.position = "right"
    )
  
  ggsave(file.path(outdir, "Functional_module_count_heatmap.png"),
         p_heatmap, width = 9, height = 7, dpi = 600)
  
  ggsave(file.path(outdir, "Functional_module_count_heatmap.pdf"),
         p_heatmap, width = 9, height = 7)
  
  # ───────────────────────────────────────────────────────
  # Plot 6: detailed module dot plot
  # Dot size = DEG count
  # Dot color = Regulation
  # ───────────────────────────────────────────────────────
  
  dot_df <- module_counts %>%
    filter(n > 0) %>%
    mutate(
      Time_Regulation = paste(Time, Regulation, sep = "_"),
      Time_Regulation = factor(
        Time_Regulation,
        levels = c(
          "24 HAI_UP", "24 HAI_DOWN",
          "48 HAI_UP", "48 HAI_DOWN",
          "96 HAI_UP", "96 HAI_DOWN"
        ),
        labels = c(
          "24 HAI\nUP",
          "24 HAI\nDOWN",
          "48 HAI\nUP",
          "48 HAI\nDOWN",
          "96 HAI\nUP",
          "96 HAI\nDOWN"
        )
      )
    )
  
  write.csv(
    dot_df,
    file.path(outdir, "Functional_module_count_matrix_for_dotplot.csv"),
    row.names = FALSE
  )
  
  p_dot <- ggplot(
    dot_df,
    aes(
      x = Time_Regulation,
      y = Functional_module,
      size = n,
      color = Regulation
    )
  ) +
    geom_point(alpha = 0.85) +
    scale_color_manual(
      values = c(
        "UP" = "#00BFC4",
        "DOWN" = "#F8766D"
      ),
      labels = c(
        "UP" = "Induced",
        "DOWN" = "Repressed"
      ),
      name = "Regulation"
    ) +
    scale_size_continuous(
      range = c(2, 10),
      breaks = c(1, 5, 10, 20, 50),
      name = "DEG count"
    ) +
    labs(
      x = "",
      y = "",
      title = "Functional module dynamics during BLS infection",
      subtitle = "Dot size indicates DEG count"
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.text.x = element_text(color = "black", size = 11),
      axis.text.y = element_text(color = "black", size = 11),
      legend.position = "right"
    )
  
  print(p_dot)
  
  ggsave(
    file.path(outdir, "Functional_module_count_dotplot.png"),
    p_dot,
    width = 9,
    height = 7,
    dpi = 600
  )
  
  ggsave(
    file.path(outdir, "Functional_module_count_dotplot.pdf"),
    p_dot,
    width = 9,
    height = 7
  )
  
  #────────────────────────────────────────────────────────────
  # Fig percentage plot
  # WITHOUT "Other / unknown"
  # WITH sample size (n)
  #────────────────────────────────────────────────────────────
  
  fig_percent <- fig_counts %>%
    filter(Fig_module != "Other / unknown") %>%
    group_by(Time, Regulation) %>%
    mutate(
      Total_n = sum(n),
      Percent = 100 * n / Total_n
    ) %>%
    ungroup()
  
  # labels
  n_labels <- fig_percent %>%
    distinct(Time, Regulation, Total_n) %>%
    mutate(label = paste0("n = ", Total_n))
  
  # plot
  p_fig_percent <- ggplot(
    fig_percent,
    aes(
      x = Time,
      y = Percent,
      fill = Fig_module
    )
  ) +
    geom_col(
      color = "black",
      linewidth = 0.25,
      width = 0.75
    ) +
    
    geom_text(
      data = n_labels,
      aes(
        x = Time,
        y = 103,
        label = label
      ),
      inherit.aes = FALSE,
      size = 4.5,
      fontface = "bold"
    ) +
    
    facet_wrap(
      ~ Regulation,
      nrow = 1,
      labeller = as_labeller(
        c(
          UP = "Upregulated",
          DOWN = "Downregulated"
        )
      )
    ) +
    
    scale_y_continuous(
      limits = c(0,108),
      breaks = seq(0,100,20),
      expand = c(0,0)
    ) +
    
    scale_fill_manual(
      values = module_colors,
      guide = guide_legend(reverse = TRUE)
    ) +
    
    labs(
      x = "",
      y = "Percentage of DEGs (%)",
      fill = "Functional module"
    ) +
    
    theme_classic(base_size = 17) +
    
    theme(
      text = element_text(color="black"),
      axis.text = element_text(color="black"),
      axis.title = element_text(color="black"),
      strip.text = element_text(face="bold", size=16),
      strip.background = element_blank(),
      
      legend.position = "right",
      legend.title = element_text(face="bold", size=15),
      legend.text = element_text(size=12)
    )
  
  print(p_fig_percent)
  
  ggsave(
    file.path(
      outdir,
      "Fig_functional_module_percentage_without_unknown_with_n.png"
    ),
    p_fig_percent,
    width = 11,
    height = 6.5,
    dpi = 600
  )
  
  ggsave(
    file.path(
      outdir,
      "Fig_functional_module_percentage_without_unknown_with_n.pdf"
    ),
    p_fig_percent,
    width = 11,
    height = 6.5
  )
  
  # ───────────────────────────────────────────────────────
  # Save CSV and Excel tables
  # ───────────────────────────────────────────────────────
  
  write.csv(
    df %>% dplyr::select(-annotation_lower),
    file.path(outdir, paste0(prefix, "_BLS_vs_Mock_DEGs_with_Fig_7modules.csv")),
    row.names = FALSE
  )
  
  write.csv(
    long_df,
    file.path(outdir, paste0(prefix, "_BLS_vs_Mock_DEGs_with_Fig_7modules_long.csv")),
    row.names = FALSE
  )
  
  write.csv(
    module_counts,
    file.path(outdir, paste0(prefix, "_functional_module_counts.csv")),
    row.names = FALSE
  )
  
  write.csv(
    fig_counts,
    file.path(outdir, paste0(prefix, "_Fig_7module_counts.csv")),
    row.names = FALSE
  )
  
  write.csv(
    fig_percent,
    file.path(outdir, paste0(prefix, "_Fig_7module_percentage.csv")),
    row.names = FALSE
  )
  
  write.csv(
    fig_percent_no_n,
    file.path(outdir, paste0(prefix, "_Fig_7module_percentage_no_n.csv")),
    row.names = FALSE
  )
  
  write.csv(
    ambiguity_check,
    file.path(outdir, paste0(prefix, "_ambiguous_module_matches.csv")),
    row.names = FALSE
  )
  
  openxlsx::write.xlsx(
    list(
      Wide_table_Fig_7modules = df %>%
        dplyr::select(-annotation_lower),
      
      Long_table_Fig_7modules = long_df,
      
      Functional_module_counts = module_counts,
      
      Fig_7module_counts = fig_counts,
      
      Fig_7module_percentage = fig_percent,
      
      Fig_7module_percentage_no_n = fig_percent_no_n,
      
      Heatmap_matrix = heat_df,
      
      Ambiguous_module_matches = ambiguity_check
    ),
    file.path(outdir, paste0(prefix, "_BLS_vs_Mock_DEGs_with_Fig_7modules.xlsx")),
    overwrite = TRUE
  )
  
  message("Finished: ", prefix)
  message("Output folder: ", outdir)
  
  invisible(list(
    df = df,
    long_df = long_df,
    module_counts = module_counts,
    fig_counts = fig_counts,
    fig_percent = fig_percent,
    fig_percent_no_n = fig_percent_no_n,
    heat_df = heat_df,
    ambiguity_check = ambiguity_check
  ))
}

# ─────────────────────────────────────────────────────────
# Run Timstein
# ─────────────────────────────────────────────────────────

timstein_results <- run_functional_module_pipeline(
  infile = "Timstein_DEGs.csv",
  outdir = "Timstein_BLS_vs_Mock_functional_modules",
  prefix = "Timstein"
)

# ─────────────────────────────────────────────────────────
# Run Boost
# ─────────────────────────────────────────────────────────

boost_results <- run_functional_module_pipeline(
  infile = "Boost_DEGs.csv",
  outdir = "Boost_BLS_vs_Mock_functional_modules",
  prefix = "Boost"
)
