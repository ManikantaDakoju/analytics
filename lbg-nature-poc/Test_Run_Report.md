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

---

## v4.2 addendum: UK Industrial Strategy added (9 November 2026)

- **Source (SRC-09):** The UK's Modern Industrial Strategy, print-ready PDF.
  - Downloaded directly from GOV.UK (35,377,105 bytes; the size matches GOV.UK's listing).
  - The file's link and SHA-256 checksum are recorded in Sources. The PDF itself is not stored in the repo because of its size.
- **Evidence:** 11 quotes, all word for word on their page; plus 2 LBG exposure rows (financial, business and other services £75.6bn; postal and telecommunications £7.1bn).
- **Growth_Sectors:** all eight IS-8 sectors added.
  - **A nature need is recorded only where a source states one:**
    - Advanced Manufacturing (water availability, p.110)
    - Clean Energy (EA framework: energy water demand)
    - Digital and Technologies (data centres: water, land, power)
  - **Five sectors are marked "None stated in sources reviewed":** Creative, Defence, Financial Services, Life Sciences, Professional and Business Services.
- **A conclusion corrected during the check:**
  - A first keyword search suggested the Industrial Strategy "barely mentions nature". Reading every hit showed that it does name nature degradation as a risk to growth (p.115), nature recovery in regulation (p.76) and a Nature Restoration Fund (p.82).
  - The accurate point, now used, is that the strategy does not set out the water or nature needs of its eight sectors (water appears only for the Oxford–Cambridge reservoirs), while the Environment Agency says growth cannot happen without water.
- **Page 2 "So what":** "Government backs 8 growth sectors (32% of the economy) and says growth cannot happen without water. Financing water and flood resilience is how banks share in that growth." Traced: C-033 (EV-090), C-034 (EV-080), C-119 (EY view).
- **Talk tracks:**
  - Page 2: the IS-8 and the reservoirs.
  - Page 3: capital deployment against the IS-8 (largest overlap: financial and business services; the water-hungry growth sectors are small or not split in LBG's disclosures) and the capability point.
- **Result:** 21 of 21 QA checks pass; 101 evidence items; 84 claims; no placeholders left in the deck.
