﻿create table synthesis_intensification.rfs_intensification_results_counties as 

SELECT 
count(a.objectid) as objectid_count,
a.atlas_stco,

--------------------------------------------------------------------------------------
-----------  areas ------------------------------------------------------------------
--------------------------------------------------------------------------------------
sum(a.acres) as acres,
sum(a.acres*0.404686) as hectares,
sum(a.acres*0.00404686) as km2,


--------------------------------------------------------------------------------------
-----------  change ------------------------------------------------------------------
--------------------------------------------------------------------------------------
--The change in acres of continuous corn in each county.

--------- CC -----------------------------------------------------------------------------------------------
sum(a.acres * (a.rot_prob_cc_rfs - a.rot_prob_cc_non_rfs)) as acres_change_rot_cc, 

--------- SS -----------------------------------------------------------------------------------------------
sum(a.acres * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs)) as acres_change_rot_ss, 

--------- WW -----------------------------------------------------------------------------------------------
sum(a.acres * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs)) as acres_change_rot_ww,

--------- CS -----------------------------------------------------------------------------------------------
sum(a.acres * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs)) as acres_change_rot_cs, 

--------- CW -----------------------------------------------------------------------------------------------
sum(a.acres * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs)) as acres_change_rot_cw, 

--------- OO -----------------------------------------------------------------------------------------------
sum(a.acres * ((a.rot_prob_ss_rfs + a.rot_prob_ww_rfs) - (a.rot_prob_ss_non_rfs + a.rot_prob_ww_non_rfs))) as acres_change_rot_oo,

--------- CO -----------------------------------------------------------------------------------------------
sum(a.acres * ((a.rot_prob_cs_rfs + a.rot_prob_cw_rfs) - (a.rot_prob_cs_non_rfs + a.rot_prob_cw_non_rfs))) as acres_change_rot_co, 



---------------------------------------------------------------------------------------
---------------change awa -------------------------------------------------------------
---------------------------------------------------------------------------------------
--change of probabilities of xx due to RFS  (county-level area-weighted average)

--------- CC -----------------------------------------------------------------------------------------------
((sum(a.acres * (a.rot_prob_cc_rfs - a.rot_prob_cc_non_rfs)))/ SUM(a.acres)*100) as acres_change_rot_cc_awa, 

--------- SS -----------------------------------------------------------------------------------------------
((sum(a.acres * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs))))/ SUM(a.acres)*100 as acres_change_rot_ss_awa, 

--------- WW -----------------------------------------------------------------------------------------------
((sum(a.acres * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs))))/ SUM(a.acres)*100 as acres_change_rot_ww_awa,

--------- CS -----------------------------------------------------------------------------------------------
((sum(a.acres * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs))))/ SUM(a.acres)*100 as acres_change_rot_cs_awa, 

--------- CW -----------------------------------------------------------------------------------------------
((sum(a.acres * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs))))/ SUM(a.acres)*100 as acres_change_rot_cw_awa, 

--------- OO -----------------------------------------------------------------------------------------------
((sum(a.acres * ((a.rot_prob_ss_rfs + a.rot_prob_ww_rfs) - (a.rot_prob_ss_non_rfs + a.rot_prob_ww_non_rfs)))))/ SUM(a.acres)*100 as acres_change_rot_oo_awa,  

--------- CO -----------------------------------------------------------------------------------------------
((sum(a.acres * ((a.rot_prob_cs_rfs + a.rot_prob_cw_rfs) - (a.rot_prob_cs_non_rfs + a.rot_prob_cw_non_rfs)))))/ SUM(a.acres)*100 as acres_change_rot_co_awa,




--------------------------------------------------------------------------------------------------
------------- water quality ----------------------------------------------------------------------
--------------------------------------------------------------------------------------------------

--------- CC -----------------------------------------------------------------------------------------------
----[NLEACH]        
----Note:units in hectares
(sum((a.acres*0.404686) * (a.rot_prob_cc_rfs - a.rot_prob_cc_non_rfs)*a.nleach_cc)) as hectares_change_rot_cc_nleach,

----[PYIELD]       
----Note:units in hectares
(sum((a.acres*0.404686) * (a.rot_prob_cc_rfs - a.rot_prob_cc_non_rfs)*a.pyield_cc)) as hectares_change_rot_cc_pyield,

----[SEDYIELD]      
----Note:unites in km2
(sum((a.acres*0.00404686) * (a.rot_prob_cc_rfs - a.rot_prob_cc_non_rfs)*a.sedyield_cc)) as km2_change_rot_cc_sedyield,




/*
--Increased xxxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.404686) * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs)*a.nleach_ss)) as hectares_change_rot_ss_nleach,
--Increased xxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.404686) * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs)*a.pyield_ss)) as hectares_change_rot_ss_pyield,
--Increased xxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.00404686) * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs)*a.sedyield_ss)) as km2_change_rot_ss_sedyield,


--Increased xxxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.404686) * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs)*a.nleach_ww)) as hectares_change_rot_ww_nleach,
--Increased xxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.404686) * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs)*a.pyield_ww)) as hectares_change_rot_ww_pyield,
--Increased xxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.00404686) * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs)*a.sedyield_ww)) as km2_change_rot_ww_sedyield,


--Increased xxxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.404686) * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs)*a.nleach_cs)) as hectares_change_rot_cs_nleach,
--Increased xxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.404686) * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs)*a.pyield_cs)) as hectares_change_rot_cs_pyield,
--Increased xxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.00404686) * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs)*a.sedyield_cs)) as km2_change_rot_cs_sedyield,


--Increased xxxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.404686) * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs)*a.nleach_cw)) as hectares_change_rot_cw_nleach,
--Increased xxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.404686) * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs)*a.pyield_cw)) as hectares_change_rot_cw_pyield,
--Increased xxxxxxx from increased continuous corn in a county.
(sum((a.acres*0.00404686) * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs)*a.sedyield_cw)) as km2_change_rot_cw_sedyield,
*/



--------- OO -----------------------------------------------------------------------------------------------
----[PYIELD]       
----Note:units in hectares
((sum((a.acres*0.404686) * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs)*a.pyield_ss))) + 
((sum((a.acres*0.404686) * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs)*a.pyield_ww))) as hectares_change_rot_oo_pyield,

----[SEDYIELD]      
----Note:unites in km2
((sum((a.acres*0.00404686) * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs)*a.sedyield_ss))) + 
((sum((a.acres*0.00404686) * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs)*a.sedyield_ww))) as km2_change_rot_oo_sedyield,



--------- CO -----------------------------------------------------------------------------------------------
----[PYIELD]       
----Note:units in hectares
((sum((a.acres*0.404686) * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs)*a.pyield_cs))) + 
((sum((a.acres*0.404686) * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs)*a.pyield_cw))) as hectares_change_rot_co_pyield,

----[SEDYIELD]      
----Note:unites in km2
((sum((a.acres*0.00404686) * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs)*a.sedyield_cs))) + 
((sum((a.acres*0.00404686) * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs)*a.sedyield_cw))) as km2_change_rot_co_sedyield,



--------- TOTAL WATER QUALITY -----------------------------------------------------------------------------------------------


----[nleach        note:units in hectates]
/*
((sum((a.acres*0.404686) * (a.rot_prob_cc_rfs - a.rot_prob_cc_non_rfs)*a.nleach_cc))) +
((sum((a.acres*0.404686) * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs)*a.nleach_ss))) + 
((sum((a.acres*0.404686) * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs)*a.nleach_ww))) +
((sum((a.acres*0.404686) * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs)*a.nleach_cs))) + 
((sum((a.acres*0.404686) * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs)*a.nleach_cw))) as hectares_change_total_nleach,
*/

----[PYIELD]       
----Note:units in hectares
((sum((a.acres*0.404686) * (a.rot_prob_cc_rfs - a.rot_prob_cc_non_rfs)*a.pyield_cc))) + 
((sum((a.acres*0.404686) * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs)*a.pyield_ss))) + 
((sum((a.acres*0.404686) * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs)*a.pyield_ww))) +
((sum((a.acres*0.404686) * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs)*a.pyield_cs))) + 
((sum((a.acres*0.404686) * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs)*a.pyield_cw))) as hectares_change_total_pyield,

----[SEDYIELD]      
----Note:unites in km2
((sum((a.acres*0.00404686) * (a.rot_prob_cc_rfs - a.rot_prob_cc_non_rfs)*a.sedyield_cc))) + 
((sum((a.acres*0.00404686) * (a.rot_prob_ss_rfs - a.rot_prob_ss_non_rfs)*a.sedyield_ss))) + 
((sum((a.acres*0.00404686) * (a.rot_prob_ww_rfs - a.rot_prob_ww_non_rfs)*a.sedyield_ww))) +
((sum((a.acres*0.00404686) * (a.rot_prob_cs_rfs - a.rot_prob_cs_non_rfs)*a.sedyield_cs))) + 
((sum((a.acres*0.00404686) * (a.rot_prob_cw_rfs - a.rot_prob_cw_non_rfs)*a.sedyield_cw))) as km2_change_total_sedyield,

a.geom


FROM synthesis_intensification.rfs_intensification_agroibis_counties as a 


GROUP BY
  a.atlas_stco,
  a.acres_calc,
  a.geom

