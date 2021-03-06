﻿SELECT 
  rfs_intensification_results_counties_test.atlas_stco, 
  rfs_intensification_results_counties_test.kg_change_total_pyield, 
  rfs_intensification_results_counties_test.kg_change_total_nleach, 
  rfs_intensification_results_counties_test.ton_change_total_sedyield, 
  rfs_intensification_results_counties.atlas_stco, 
  rfs_intensification_results_counties.kg_change_total_pyield, 
  rfs_intensification_results_counties.kg_change_total_nleach, 
  rfs_intensification_results_counties.ton_change_total_sedyield
FROM 
  intensification_ksu.rfs_intensification_results_counties_test, 
  intensification_ksu.rfs_intensification_results_counties
WHERE 
  rfs_intensification_results_counties_test.atlas_stco = rfs_intensification_results_counties.atlas_stco;



  SELECT 
  rfs_intensification_results_counties_test.atlas_stco, 
  rfs_intensification_results_counties_test.kg_change_total_pyield - rfs_intensification_results_counties.kg_change_total_pyield,
  rfs_intensification_results_counties_test.kg_change_total_nleach - rfs_intensification_results_counties.kg_change_total_nleach, 
  rfs_intensification_results_counties_test.ton_change_total_sedyield - rfs_intensification_results_counties.ton_change_total_sedyield, 
  rfs_intensification_results_counties.atlas_stco
FROM 
  intensification_ksu.rfs_intensification_results_counties_test, 
  intensification_ksu.rfs_intensification_results_counties
WHERE 
  rfs_intensification_results_counties_test.atlas_stco = rfs_intensification_results_counties.atlas_stco;
