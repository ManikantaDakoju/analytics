# Test run report: Copilot toolkit v4 → v4.1 (9 November 2026)

**Method.** Claude acted as the Copilot agent:
- It followed `01_Agent_Instructions.md` and `03_Run_Prompts.md` exactly, using the four LBG documents plus two UK government documents.
- Each step's output was pasted into the master template the way an analyst would.
- The workbook then filled the deck skeleton.

**Caveat.** This tests the instructions, workbook and skeleton. How Copilot itself behaves still needs your own run.

## Results by prompt

| Prompt | Result | Issue found | Fix in v4.1 |
|---|---|---|---|
| 0 Start | Pass | – | – |
| 1 Sources | Pass | – | Sources now also hold SRC-07 (AI Opportunities Action Plan) and SRC-08 (EA water framework). The AI plan's printed page = PDF − 1; the EA framework's printed page = PDF page. |
| 2 Evidence | Pass | All 59 existing PDF quotes still match their PDF page word for word | – |
| 3 Exposure | Pass | – | – |
| **3b Growth (new)** | Pass | 12 new quotes, all word for word on their PDF page. 4 growth sectors linked to evidence and LBG exposure | New prompt and Growth_Sectors tab. **It has to run after Prompt 3**, because it links to exposure IDs, so it is called 3b (not 2b) |
| 4 Mapping | Pass | – | New evidence type "Growth priority" |
| 5 Scores | **Friction** | The output is one 7-column table, but it had to be pasted into six non-adjacent columns (E, F, H, P, Q, R). That is error-prone. | New paste area at Scoring!W5. Columns E, F, H, P, Q and R look values up by SubTheme_ID, so row order doesn't matter. |
| 6 Glossary and calculations | **Friction** | Calculation formulas pointed at cell row numbers, so they break if rows move or are pasted in a different order | Formulas now look up by Evidence_ID, e.g. `=VLOOKUP("EV-040",Client_Exposure!$A:$E,5,FALSE)` |
| 7 Slide text and claims | **Friction** | Values had to be pasted in exactly the template's key order; one row out of place shifts everything | New paste area at Slide_Content!J5. Columns B and C look up by key. |
| 7 (check) | **Caught** | Tile 4 text was 91 characters (limit 85). Shortening it nearly put non-verbatim words in quotation marks. | Text shortened, quotation marks removed (exact quote kept in the claim). New writing rule: quotation marks only around exact source words. |
| 8 QA | Pass | **21 of 21 checks pass** (88 evidence items, 83 claims, every calculated figure matches its formula) | 2 new checks: Prompt 5 pasted for all 15 sub-themes; Growth_Sectors evidence exists |
| 9 Fill deck | Pass | No placeholders left. Speaker notes list every claim with its source and page. | Page 2 redesigned with the context band (see below) |

## Page 2 changes (v4.1)
- **New context band, three blocks:**
  - **What it is:** what an NBSAP is, and the UK's plan (written by Excel)
  - **What we measured:** 34 commitments, c.£60bn, 5 themes (written by Excel)
  - **Why it matters:** LBG's own figure of a £56bn nature finance gap, and the role of private finance (traced)
- **Tile 3** is now the Environment Agency's **5bn litres a day public water shortfall by 2055**. The CCC £11bn adaptation gap was dropped from the slide.
- **Tile 4** was an EY view and is now a government source: the Environment Agency says housing, net zero, food and data centres "All of these require water".
- The chart subtitle no longer repeats "34 commitments, c.£60bn".

## Still open
1. **UK Modern Industrial Strategy** (the upload failed): needed to cite the full list of eight priority sectors.
2. **ENCORE extract** (the upload failed): needed for sector-to-nature dependencies beyond water, land and power.
3. **LBG Annual / Sustainability Report 2025:** to confirm the "not yet visible" items and subsidence.
4. **The points Helen thought looked wrong** in the earlier review.
5. **Your own Copilot run**, to see how Copilot itself handles Prompts 3b, 5 and 7.
