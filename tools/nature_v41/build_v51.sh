#!/usr/bin/env bash
# Rebuild v5.1 (after the 19 November review) on top of v5.0, then run the mock test from the blank v5.1 master.
# Usage: bash build_v51.sh WORKDIR   (run from tools/nature_v41)
set -euo pipefail
W=${1:?workdir}; RC=/mnt/skills/public/xlsx/scripts/recalc.py
bash build_v50.sh "$W/v50" >/dev/null
python3 patch_v51.py "$W/v50/master_v50.xlsx" "$W/master_v51.xlsx"
python3 $RC "$W/master_v51.xlsx" 120 | grep -q '"total_errors": 0'
python3 lbg_v51.py "$W/v50/LBG_v50.xlsx" "$W/LBG_v51.xlsx"
python3 $RC "$W/LBG_v51.xlsx" 180 | grep -q '"total_errors": 0'
python3 apply_review_v51.py base_v51/LBG_styled_by_reviewer_v5.0.pptx --skeleton "$W/skel_v51.pptx"
python3 apply_review_v51.py base_v51/LBG_styled_by_reviewer_v5.0.pptx "$W/LBG_v51.xlsx" "$W/LBG_styled_direct_v51.pptx"
python3 fill_deck.py "$W/skel_v51.pptx" "$W/LBG_v51.xlsx" "$W/deck_tmp.pptx"
python3 fill_charts.py "$W/deck_tmp.pptx" "$W/LBG_v51.xlsx" "$W/LBG_deck_v51.pptx"
rm -rf "$W/mock"; python3 mock_run.py "$W/master_v51.xlsx" "$W/LBG_v51.xlsx" "$W/mock" "$W/mock_from_blank.xlsx"
python3 $RC "$W/mock_from_blank.xlsx" 180 | grep -q '"total_errors": 0'
python3 fill_deck.py "$W/skel_v51.pptx" "$W/mock_from_blank.xlsx" "$W/m_tmp.pptx"
python3 fill_charts.py "$W/m_tmp.pptx" "$W/mock_from_blank.xlsx" "$W/mock_deck.pptx"
python3 compare_mock.py "$W" 51
