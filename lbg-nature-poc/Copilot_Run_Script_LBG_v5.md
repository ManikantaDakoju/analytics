# Copilot run script: Lloyds Banking Group, UK (toolkit v5.0)

This is the LBG run, ready to paste into the Copilot agent. Send one prompt at a time and check each answer before going on. After each answer, compare it with the expected output in `mock_test_v5/`. The Evidence_IDs and wording will differ, but the content should match. Then paste the answer where shown.

The generic prompts are in `copilot-nature-agent/03_Run_Prompts.md`. This script fills them in for LBG.

## Before you start (about 20 minutes, once)

1. **Agent.** Create or update the agent "Nature Opportunity Analyst":
   - Instructions: paste `01_Agent_Instructions.md` (v5.0).
   - Knowledge: upload `02_Methodology_and_Specifications.docx` and `04_Slide_Writing_Guide_and_Example.docx` (both v5.0), replacing the v4.5 files.
   - Web search: off.
2. **Workbook.** Copy `Nature_Opportunity_Master_Template.xlsx` (v5.0) to `LBG_UK_Nature_Mapping.xlsx`.
3. **Files to attach in the chat** (not as Knowledge):
   - `working-with-nature-report.pdf`
   - `lbg-built-to-last-report.pdf`
   - `2025-lbg-sustainability-data-sheet.xlsx`
   - `2025-lbg-sustainable-financing-framework.pdf`
   - LBG Sustainability Report 2025, if you can attach it. If not, the run still works: QA shows one Warning and slide 4 keeps its caveat.
4. **Files for Prompt 3b only:**
   - `United_Kingdom_ai_opportunities_action_plan.pdf`
   - `United_Kingdom_2025_EA_National_Framework_Water_Resources_summary.pdf`
   - The Industrial Strategy. The full PDF is about 35MB; if Copilot rejects it, attach the executive summary and the sector list on page 115.
   - The ENCORE Insights country data (`ENCORE_Insights_Country_Dashboard_Data_v3.0.zip`, or its two CSV files).

---

### Prompt 0
```
Start a new nature opportunity assessment.
CLIENT: Lloyds Banking Group   CLIENT_SHORT: LBG
COUNTRY_ID: GBR   COUNTRY_LABEL: UK
SECTOR: Banking and financial services
AS_AT_DATE: 16/11/2026   MEETING_DATE: 19 November 2026   COVER STATUS: Draft for internal review
I have attached: working-with-nature-report.pdf; lbg-built-to-last-report.pdf; 2025-lbg-sustainability-data-sheet.xlsx; 2025-lbg-sustainable-financing-framework.pdf<; LBG Sustainability Report 2025 if attached>.
Confirm the inputs back to me. Check the core client sources: (1) latest annual report and/or sustainability report, (2) sustainability or ESG data sheet, (3) sustainable / green financing framework, (4) any nature, TNFD or climate report. Tell me which are missing. Do not start the analysis yet.
```
- **Expected:** `mock_test_v5/P0_inputs.md`
- **Do:** fill in the Inputs tab.

### Prompt 1
```
Step 1. List every attached client document as a Sources table with the exact headers in SRC-METH section 8. Use these IDs: SRC-01 Working with nature (2024); SRC-02 Built to Last (2026); SRC-04 Sustainability Data Sheet 2025; SRC-05 Sustainable Financing Framework 2025; SRC-06 Annual Report 2025 and Sustainability Report 2025 (if not attached: Included = N, reason "not provided"). In "Page numbering", state how printed page numbers relate to PDF page numbers for each file. Use the Type value "Annual / sustainability report" exactly for SRC-06. Flag any document older than 24 months or that you could not read in full.
```
- **Expected:** `P1_sources.md`
- **Check:**
  - SRC-01: printed = PDF.
  - SRC-02: no printed numbers, so PDF pages are used.
  - SRC-05: printed = PDF − 1.
  - SRC-04: tabs.
- **Paste:** at Sources!A7. (SRC-07 to SRC-10 are added in Prompt 3b.)

### Prompt 2 (four times, one per source)
```
Step 2 for SRC-01. Read the whole document. Produce Evidence_Log rows (start at EV-001) for every nature-related commitment, target, policy, exposure, financing activity, product, eligibility criterion, partnership, metric or disclosure, and for regulations, market drivers or national figures the document cites. For each row give the printed page and the PDF page. Quotes must be copied exactly, 25 words or fewer, with no paraphrasing inside the quote. Return the table only, then tell me the last Evidence_ID used.
```
Repeat for SRC-02, SRC-05 and SRC-04, each time continuing from the last Evidence_ID. For SRC-06, if attached, add: *"also look specifically for (a) how LBG defines nature priority sectors (around page 76); (b) physical-risk analysis: flood, drought, subsidence and the mortgage book; (c) sustainable finance totals and categories; (d) nature targets, metrics or TNFD disclosures."*
- **Expected:** `P2_evidence_SRC-01.md`, `…SRC-02.md`, `…SRC-05.md`, `…SRC-04.md`
- **Check:** spot-check 5 quotes per source against the PDF page.
- **Paste:** at the next empty Evidence_Log row.

### Prompt 3
```
Step 3. From SRC-04 (tab "18 - Environmental risk", 31 December 2025 columns), produce Client_Exposure rows exactly as disclosed: all 12 sector totals (they must add up to the Group total of 645,058), the Agriculture and Housebuilders sub-sector rows, the "Nature priority sectors" subtotal, and the Group total row. For each sector total also give "Nature priority sector % of Group" and "Increased climate risk % of Group" as fractions (e.g. 0.011). Give each row its own Evidence_ID (continue from EV-0xx) and also give me the matching Evidence_Log rows, quoting the whole row (lending | off-balance sheet | total | climate % | nature %). Also log footnote 11 on mortgages as a Disclosure. Use the Row type values in SRC-METH section 8. Suggest linked themes and label them "EY view". Do not convert currencies or estimate missing values.
```
- **Expected:** `P3_exposure.md`
- **Check:** QA 26 shows Pass, meaning the sector totals add up to the Group total.
- **Paste:** Client_Exposure at A5 (columns A:L; never column M). Evidence rows into Evidence_Log.

### Prompt 3b (national growth priorities; reuse for later UK clients)
```
Step 3b for SRC-07 (AI Opportunities Action Plan), SRC-08 (Environment Agency National Framework for Water Resources 2025, summary), SRC-09 (The UK's Modern Industrial Strategy 2025) and SRC-10 (ENCORE Insights Country Dashboard data v3.0). Add these four to my Sources table first (SRC-10 title must carry the full Global Canopy citation, CC BY-SA 4.0). These are national government strategies. (a) Produce Evidence_Log rows (continue from EV-0xx), Evidence type "Growth priority", Scope "External driver", for every growth sector the government names as a priority and every statement of what those sectors need from nature (water, land, flood protection, power). Quotes exact, 25 words or fewer, printed and PDF page. (b) Then produce Growth_Sectors rows with the exact headers in SRC-METH section 8. Only include a dependency that a source states; never infer one. (c) From the ENCORE data add Evidence_Log rows for the UK headline nature-dependency figure and, for each growth sector, the ENCORE sector's water supply, water flow regulation and flood mitigation scores, quoted exactly from the CSV row. Label these "ENCORE (modelled, 0–1)" in Growth_Sectors. If no source states a nature need for a priority sector, write "None stated in sources reviewed". Return the tables only.
```
- **Expected:** `P3b_growth.md`
- **Check:** Growth_Sectors column I shows OK on every row.
- **Paste:** Sources rows, Evidence_Log rows, then Growth_Sectors at A5.

### Prompt 4
Use the generic Prompt 4. Paste the GBR rows of NBSAP_Commitments (columns A, E, G).
- **Expected:** `P4_mapping.md`
- **Check:** the classification review should propose M01 and M09 → W02, and M120 and M121 → DM01.
- **Paste:** mapping at A5 (columns A and C:H). Approved overrides go in NBSAP_Commitments N:O.

### Prompts 5 and 5b
Use the generic prompts.
- **Expected:** `P5_scores.md` and `P5b_alignment.md`. Alignment: Yes only for W and DM (EV for Built to Last PDF page 3).
- **Paste:** Prompt 5 at Scoring!W5; Prompt 5b at Theme_View T5:U9.
- **Check:** **Review every score.**

### Prompt 6
Use the generic Prompt 6 (v5.0 asks for the Key text column).
- **Expected:** `P6_glossary_calcs.md`. Abbreviations to keep: LBG, UK, PRA, SS5/25, AI. Everything else is spelt out.
- **Paste:** Glossary rows from row 60 (columns A:G); Calculations from row 30. Include the nature-priority share, i.e. "Nature priority sectors" subtotal ÷ Group total.

### Prompt 7
Use the generic Prompt 7, and paste back the five tables it asks for. Add this line:
```
Cover: cover_date = "Draft for internal review – 19 November 2026". Section labels: s2_section "UK nature finance and LBG"; s3_section "LBG portfolio exposure and position".
```
- **Expected:** `P7_slides_claims.md`
- **Check:**
  - Slide_Content columns G and H are all OK.
  - Every client share has one decimal place (56.7%, not 57%).
- **Paste:** Slide_Content at J5; claims at Claims_Register A5.

### Prompt 8
Use the generic Prompt 8 until QA_Log shows 25 Pass and 1 Warning (check 22, Sustainability Report not reviewed). If SRC-06 was attached and logged, all 26 should pass.

### Prompt 9 (PowerPoint)
1. Copy `Nature_Deck_Skeleton_EY.pptx` (v5.0).
2. **9b:** Run the Copilot-in-PowerPoint prompt with Deck_Tokens A5:B down. Columns C, D and G:I are helpers; do not copy them.
3. **9c:** Paste the four charts:
   - A5:C10: page 2
   - A13:C18: page 3, score
   - A21:C26: page 3, money
   - A29:C41: page 4, book by sector
4. **9d:** The EY team writes slide 5.

**Final check:**
- No `{{` left.
- No `[EY team` left (once slide 5 is written).
- Each of pages 2–4 ends with an "Abbreviations:" line.
- Compare with `LBG_Nature_Opportunities_v5.0.pdf`.
