# ES 950: Lake Quality Project
Group project for ES 950 to analyze data from the DNR on Wisconsin lake quality.

## Parcel dataset
### Variable definitions
* PARCELID: unique ID of the parcel being assessed; comes from [statewide parcel data](https://www.sco.wisc.edu/parcels/data/)
* CANOPY_PCT: percentage of the Riparian Buffer Zone covered by tree canopy
  * Number between 0 and 100
* SHRUB_PRESENCE: presence of shrubs in the Riparian buffer zone (related to SHRUB_HERB_PCT)
  * 1 if present, 0 if not present
* HERB_PRESENCE: presence of herbaceous plants in the Riparian Buffer Zone (related to SHRUB_HERB_PCT)
  * 1 if present, 0 if not present
* SHRUB_HERB_PCT: percentage of the ground layer in the Riparian Buffer Zone covered by shrubs and/or herbaceous plants combined
  * Number between 0 and 100
* IMPERVIOUS_PCT: percentage of the ground layer in the Riparian Buffer Zone covered by impervious surface (e.g., concrete, decking, compacted gravel, etc.)
  * Number between 0 and 100
* MANI_LAWN_PCT: percentage of the ground layer in the Riparian Buffer Zone covered by manicured lawn (lawn that is trimmed short)
  * Number between 0 and 100
* AG_PCT: percentage of the ground layer in the Riparian Buffer Zone covered by agriculture (e.g., row crops, hay field, etc.)
  * Number between 0 and 100
* OTHER_PCT: percentage of the ground layer in the Riparian Buffer Zone covered by other materials (e.g., bare soil, mulch, gravel, etc.)
  * Number between 0 and 100
* BUILDINGS_CNT: number of buildings present in the Riparian Buffer Zone
  * Number
* BOAT_SHORE_CNT: number of boats flipped upside down on the shore present in the Riparian Buffer Zone
  * Number
* FIRE_PIT_CNT: number of fire pits present in the Riparian Buffer Zone
  * Number
* OTHER_STRUCTURE_CNT: number of other structures in the Riparian Buffer Zone
  * Number
* POINT_SOURCE_PRES: presence of a point source (e.g., culverts, drain pipes, sump pumps, rain gutters, etc.)
  * 2 if present in the Riparian Buffer Zone, 1 if present outside of the Riparian Buffer Zone, 0 if not present
* CHANNEL_FLOW_PRES: presence of channelized flow or gullies
  * 2 if present in the Riparian Buffer Zone, 1 if present outside of the Riparian Buffer Zone, 0 if not present
* STAIR_LAKE_PRES: presence of stairs, trails, or roads leading directly to the top of the bank lip
  * 2 if present in the Riparian Buffer Zone, 1 if present outside of the Riparian Buffer Zone, 0 if not present
* LAWN_LAKE_PRES: presence of sloped lawn or soil such that runoff leads directly into the lake
  * 2 if present in the Riparian Buffer Zone, 1 if present outside of the Riparian Buffer Zone, 0 if not present
* SAND_DEP_PRES: presence of sand or silt deposits
  * 2 if present in the Riparian Buffer Zone, 1 if present outside of the Riparian Buffer Zone, 0 if not present
* OTHER_RUNOFF_PRES: other runoff factors present (may include presence of bare soil)
  * 2 if present in the Riparian Buffer Zone, 1 if present outside of the Riparian Buffer Zone, 0 if not present
* VERTICAL_WALL_LEN: length of vertical sea wall in the Bank Zone
  * Number (recorded in feet)
* RIPRAP_LEN: length of rip rap in the Bank Zone
  * Number (recorded in feet)
* EROSION_CNTRL_LEN: length of other erosion control structures in the Bank Zone
  * Number (recorded in feet)
* ART_BEACH_LEN: length of artificial beach in the Bank Zone
  * Number (recorded in feet)
* GREAT_ERO_LEN: length of slumping banks or bank erosion when the bank face is greater than 1 foot high
  * Number (recorded in feet)
* LESS_ERO_LEN: length of slumping banks or bank erosion when the bank face is less than 1 foot high
  * Number (recorded in feet)
* PIERS_CNT: number of piers present in the Littoral Zone
  * Number
* BOAT_LIFT_CNT: number of boat lifts present in the Littoral Zone
  * Number
* SWIM_RAFT_CNT: number of swim rafts present in the Littoral Zone within 50 feet from shore
  * Number
* BOATHOUSE_CNT: number of boathouses present in the Littoral Zone
  * Number
* MARINAS_CNT: number of marinas present in the Littoral Zone
  * Number   
* STRUCTURE_OTHER_CNT: number of other structures present in the Littoral Zone
  * Number
* EMERGENT_VEG_PRES: presence of emergent aquatic plants in the Littoral Zone
  * 1 if present, 0 if not present
* FLOATING_VEG_PRES: presence of rooted floating aquatic plants in the Littoral Zone
  * 1 if present, 0 if not present
* PLANT_REMOVAL_PRES: presence of aquatic plant removal areas in the Littoral Zone
  * 1 if present, 0 if not present
* FLOAT_EMERG_PRES: presence of both emergent and rooted aquatic plants in the Littoral Zone
  * 1 if present, 0 if not present
* EXPOSED_CANOPY_PRES: presence of tree canopy on an Exposed Lake Bed
  * 1 if present, 0 if not present
  * Only documented if at least 3 horizontal feet of the lake bed are exposed
* EXPOSED_SHRUB_PRES: presence of shrubs on an Exposed Lake Bed
  * 1 if present, 0 if not present
  * Only documented if at least 3 horizontal feet of the lake bed are exposed
* EXPOSED_HERB_PRES: presence of herbaceous plants on an Exposed Lake Bed
  * 1 if present, 0 if not present
  * Only documented if at least 3 horizontal feet of the lake bed are exposed
* EXPOSED_MOW_PRES: evidence of mowing on an Exposed Lake Bed
  * 1 if present, 0 if not present
  * Only documented if at least 3 horizontal feet of the lake bed are exposed
* EXPOSED_TILL_PRES: evidence of tilling or digging on an Exposed Lake Bed
  * 1 if present, 0 if not present
  * Only documented if at least 3 horizontal feet of the lake bed are exposed
* NOTES: any notes documented by the assessor
  * Free text, "NA" if no notes documented
* PLACENAME: town where the lake is located
* COUNTY: county in Wisconsin where the lake is located
* LAKE_NAME: name of the lake
* WBIC: Water Body Identification Code for the lake

## Woody habitat dataset
### Variable definitions
* WBIC: Water Body Identification Code for the lake
* LAKE_NAME: name of the lake
* UNIQUE_ID: unique identifier for the log being documented
* BRANCH: branchiness ranking of the piece of wood
  * 0 if no branches, 1 if a few branches, 2 if tree trunk has a full crown
* TOUCH_SHORE: whether the log crosses the high water line (i.e., comes out of the water onto the shore)
  * 1 if crosses HWL, 0 if not
* IN_WATER: whether the log is currently underwater
  * 1 if currently underwater, 0 if log is below HWL but less than 5 feet of the log is underwater
* LATITUDE: latitude coordinate of the log's location
* LONGITUDE: longitude coordinate of the log's location

