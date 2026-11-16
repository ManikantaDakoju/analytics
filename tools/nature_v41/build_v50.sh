#!/usr/bin/env bash
# Rebuild the v5.0 toolkit and LBG example from v4.5, then run the mock test from the blank master.
# Usage: bash build_v50.sh WORKDIR   (run from tools/nature_v41; needs LibreOffice and node_modules)
set -euo pipefail
W=${1:?workdir}; R=../..; RC=/mnt/skills/public/xlsx/scripts/recalc.py
mkdir -p "$W"
python3 build_skeleton_v50.py base_v45/Nature_Deck_Skeleton_EY.pptx "$W/skel_v50.pptx" "$W/build" >/dev/null
python3 patch_v50.py base_v45/Nature_Opportunity_Master_Template.xlsx "$W/master_v50.xlsx"
python3 $RC "$W/master_v50.xlsx" 120 | grep -q '"total_errors": 0'
python3 lbg_v50.py $R/lbg-nature-poc/archive/LBG_UK_Nature_Mapping_v4.5.xlsx $R/lbg-nature-poc/inputs/2025-lbg-sustainability-data-sheet.xlsx "$W/LBG_v50.xlsx"
python3 $RC "$W/LBG_v50.xlsx" 180 | grep -q '"total_errors": 0'
python3 fill_deck.py "$W/skel_v50.pptx" "$W/LBG_v50.xlsx" "$W/deck_tmp.pptx"
python3 fill_charts.py "$W/deck_tmp.pptx" "$W/LBG_v50.xlsx" "$W/LBG_deck_v50.pptx"
rm -rf "$W/mock"; python3 mock_run.py "$W/master_v50.xlsx" "$W/LBG_v50.xlsx" "$W/mock" "$W/mock_from_blank.xlsx"
python3 $RC "$W/mock_from_blank.xlsx" 180 | grep -q '"total_errors": 0'
python3 fill_deck.py "$W/skel_v50.pptx" "$W/mock_from_blank.xlsx" "$W/m_tmp.pptx"
python3 fill_charts.py "$W/m_tmp.pptx" "$W/mock_from_blank.xlsx" "$W/mock_deck.pptx"
python3 compare_mock.py "$W"
