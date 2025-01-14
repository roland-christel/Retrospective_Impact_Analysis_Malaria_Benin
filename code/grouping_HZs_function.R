group_districts <- function(df) {
  # Définir les groupements de districts
  district_groups <- list(
    "Kandi-Gogounou-Ségbana" = c("Kandi", "Gogounou", "Segbana"),
    "Tanguiéta-Cobly-Matéri" = c("Tanguieta", "Kobli", "Materi"),
    "Kouandé-Péhunco-Kérou" = c("Kouande", "Pehunco", "Kerou"),
    "Djougou-Ouaké-Copargo" = c("Djougou", "Ouake", "Copargo"),
    "Sakété-Ifangni" = c("Sakete", "Ifangni"),
    "Pobè-Kétou-Adja-ouèrè" = c("Pobe", "Ketou", "Adja-Ouere"),
    "Abomey-Calavi-So-ava" = c("Abomey-Calavi", "So-Ava"),
    "Djidja-Abomey-Agbangnizoun" = c("Djidja", "Abomey", "Agbangnizoun"),
    "Savè-Ouèssè" = c("Save", "Ouesse"),
    "Comè-Grand popo-Houèyogbé-Bopa" = c("Come", "Grand-Popo", "Houeyogbe", "Bopa"),
    "Klouékamè-Toviklin-Lalo" = c("Klouekanme", "Toviklin", "Lalo"),
    "Allada-Toffo-Zè" = c("Allada", "Toffo", "Ze"),
    "Akpro-missérété-Avrankou-Adjarra" = c("Akpo-Misserete", "Avrankou", "Adjarra"),
    "Bohicon-Za-kpota-Zogbodomey" = c("Bohicon", "Za-Kpota", "Zogbodomey"),
    "Aplahoué-Djakotomey-Dogbo" = c("Aplahoue", "Djakotomey", "Dogbo"),
    "Adjohoun-Bonou-Dangbo" = c("Adjohoun", "Bonou", "Dangbo"),
    "Lokossa-Athiémè" = c("Lokossa", "Athieme"),
    "Ouidah-Kpomassè-Tori-Bossito" = c("Ouidah", "Kpomasse", "Tori-Bossito"),
    "Tchaourou (ZS)" = "Tchaourou",
    "Porto-Novo-Sèmè-kpodji-Aguégués" = c("Porto-Novo", "Seme-Kpodji", "Aguegues"),
    "Covè-Zagnanado-Ouinhi" = c("Cove", "Zagnanado", "Ouinhi"),
    "Savalou-Bantè" = c("Savalou", "Bante"),
    "Bassila (ZS)" = "Bassila",
    "Dassa-Glazoué" = c("Dassa-Zoume", "Glazoue"),
    "Banikoara (ZS)" = "Banikoara",
    "N'Dali-Parakou" = c("N'dali", "Parakou"),
    "Malanville-Karimama" = c("Malanville", "Karimama"),
    "Nikki-Kalalé-Pèrèrè" = c("Nikki", "Kalale", "Perere"),
    "Bembèrèkè-Sinendé" = c("Bembereke", "Sinende"),
    "Natitingou-Boukoumbé-Toucountouna" = c("Natitingou", "Boukombe", "Toucountouna"),
    "Cotonou" = 'Cotonou'
  )
  
  # Créer un vecteur de correspondance pour le mapping
  district_mapping <- stack(district_groups) %>%
    select(district = values, group = ind) %>%
    deframe()
  
  # Appliquer les groupements et calculer les moyennes
  grouped_df <- df %>%
    mutate(district_group = district_mapping[districts]) %>%
    group_by(month_start,period, district_group) %>%
    summarise(
      temp_cds_avg = mean(temp_cds_avg, na.rm = TRUE),
      precip_cds_sum = mean(precip_cds_sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(grouped_df)
}
