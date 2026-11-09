# Run prompts (v4.3) – copy into the Copilot agent, one at a time

Replace the text in <angle brackets>. After each answer:
1. Check the answer.
2. Paste the tables into your copy of the master template.
3. Save the workbook, then send the next prompt.

Attach the client documents in this chat, not in the agent's Knowledge.

---

### Prompt 0 – Start a new client
```
Start a new nature opportunity assessment.
CLIENT: <Lloyds Banking Group>   CLIENT_SHORT: <LBG>
COUNTRY_ID: <GBR>   COUNTRY_LABEL: <UK>
SECTOR: <Banking and financial services>
AS_AT_DATE: <05/11/2026>   MEETING_DATE: <10th December 2026>
I have attached: <list the files>.
Confirm the inputs back to me and tell me if anything is missing. Do not start the analysis yet.
```

### Prompt 1 – Sources
```
Step 1. List every attached client document as a Sources table with the exact headers in SRC-METH section 8. Number them SRC-01, SRC-02 … in the order listed. In "Page numbering", state how printed page numbers relate to PDF page numbers for each file (e.g. "printed = PDF − 1", or "no printed numbers: PDF page used"; for spreadsheets "tabs"). Flag any document older than 24 months or that you could not read in full.
```
*Paste into Sources from row 7. SRC-EY and SRC-METH are already there.*

### Prompt 2 – Evidence (repeat for each source)
```
Step 2 for <SRC-01>. Read the whole document. Produce Evidence_Log rows (start at <EV-001>) for every nature-related commitment, target, policy, exposure, financing activity, product, eligibility criterion, partnership, metric or disclosure, and for regulations, market drivers or national figures the document cites. For each row give the printed page and the PDF page. Quotes must be copied exactly, 25 words or fewer, with no paraphrasing inside the quote. Return the table only, then tell me the last Evidence_ID used.
```
*Tip: for long reports, add "PDF pages <x–y> only" and run it in chunks. Spot-check 5 quotes against the PDF, using the PDF page.*

### Prompt 3 – Portfolio exposure
```
Step 3. From <SRC-0x> (<tab or page of the exposure / concentration / nature-priority table>), produce Client_Exposure rows exactly as disclosed, including the group total row. Give each row its own Evidence_ID (continue from <EV-0xx>) and also give me the matching Evidence_Log rows. Use the Row type values in SRC-METH section 8. Suggest linked themes and label them "EY view". Do not convert currencies or estimate missing values.
```

### Prompt 3b – National growth priorities (once per country)
Attach the country's official strategies, e.g. for the UK:
- The UK's Modern Industrial Strategy (GOV.UK collection "The UK's Modern Industrial Strategy 2025"; the print-ready PDF is about 35MB, so if Copilot rejects it, attach the executive summary pages and the sector list on p.115)
- the AI Opportunities Action Plan
- the Environment Agency National Framework for Water Resources 2025

Use official national documents only, no more than 3 years old: growth or industrial strategy, sector plans, national water or infrastructure plans. No media.
```
Step 3b for <SRC-07, SRC-08>. These are national government strategies. (a) Produce Evidence_Log rows (continue from <EV-0xx>), Evidence type "Growth priority", Scope "External driver", for every growth sector the government names as a priority and every statement of what those sectors need from nature (water, land, flood protection, power). Quotes exact, 25 words or fewer, printed and PDF page. (b) Then produce Growth_Sectors rows: Growth_ID (GS-01 …) | Growth sector (as named in the source) | Priority Evidence_IDs | Nature dependency (as stated in the source) | Dependency Evidence_IDs | Linked themes (EY view) | Client exposure link (Evidence_IDs from Client_Exposure, or "Not split") | Note. Only include a dependency that a source states; never infer one. (c) If I attach the ENCORE Insights Country Dashboard data, add Evidence_Log rows for the country's headline nature-dependency figure and, for each growth sector, the ENCORE sector's water supply, water flow regulation and flood mitigation scores, quoted exactly from the CSV row (Printed page / tab = file name and row/column). Label these "ENCORE (modelled, 0–1)" in Growth_Sectors: they are modelled sector averages, not stated requirements. If no source states a nature need for a priority sector, still list the sector and write "None stated in sources reviewed" in Dependency Evidence_IDs, with a Note. Return the two tables only.
```
*Runs after Prompt 3, so the exposure Evidence_IDs exist. Paste the evidence into Evidence_Log and the growth rows into Growth_Sectors from row 5. Column I must show OK.*

### Prompt 4 – Mapping (and optional classification review)
```
Step 4. Map every Evidence_ID to the taxonomy as NBSAP_Mapping rows (exact headers in SRC-METH section 8). Use XC only for group-level or cross-cutting items.
Optional: look at the NBSAP_Commitments rows for COUNTRY_ID that I paste below and propose a classification review only where the EY tag clearly misrepresents the commitment (Example_ID | Current SubTheme_ID | Proposed SubTheme_ID | Rationale (EY view)).
<paste NBSAP_Commitments rows for the country: columns A, E, G>
```
*Put approved changes in NBSAP_Commitments columns N and O.*

### Prompt 5 – Judgement scores
```
Step 5. For all 15 SubTheme_IDs produce: SubTheme_ID | C1 | C2 | C4 | Evidence_IDs | Rationale | Client exposure (as disclosed). Use the rubric in SRC-METH section 4, apply the C2 cap, cite Evidence_IDs for every score and label sector reasoning "EY view". Where there is no evidence, say so and score C1 = 1.
```
*Paste the whole table into the Scoring tab's paste area (cell W5, columns W:AC). Row order doesn't matter: columns E, F, H, P, Q and R look it up.*

### Prompt 6 – Glossary and calculations
```
Step 6. (a) List every abbreviation and defined term you expect to use on the slides that is not already in the Glossary (I paste the current Glossary IDs below). For each: ID (ABB- or DEF-) | Term | Type | Definition / expansion | Source_ID | Reference (page/tab). The expansion must come from a source; if no source spells it out, say so and suggest wording that avoids the abbreviation.
(b) For every client percentage or total the slides need (e.g. a sector as % of total exposure), give a Calculations row: CALC_ID (CALC-51 …) | Description | Excel formula that looks figures up by Evidence_ID, e.g. =VLOOKUP("EV-040",Client_Exposure!$A:$E,5,FALSE)/VLOOKUP("EV-067",Client_Exposure!$A:$E,5,FALSE) | Unit | How it is calculated | Input trace IDs | Source_ID.
<paste Glossary column A and the Client_Exposure table>
```
*Paste Glossary rows from row 60 and Calculations rows from row 30. Save, so the values calculate.*

### Prompt 7 – Slide text and claims
```
Step 7. Here are the workbook tables:
Theme_View (columns A–P): <paste>
Scoring sorted by Rank (columns A–L): <paste>
Calculations (columns A–C): <paste>
Growth_Sectors (columns A–H): <paste>
Deck_Tokens rows for s1_ctx_what, s1_ctx_measured, s1_left_sub, s1_callout_stat, r1–r5 and h1–h3: <paste>
Write the text for pages 2–4 as two tables.
(a) Slide_Content: Key | Value | Claim_IDs, for exactly the keys in SRC-METH section 10.
(b) Claims_Register: Claim_ID | Slide | Slide_Content key | Claim | Claim type | Primary trace ID | Other trace IDs | Derivation / how checked, for every claim (C-001 …), plus "auto:" claims for the Excel-written text listed above.
Rules: page 2 context band: s1_ctx_why says why national nature spending matters for the client's sector, quoted from evidence. Page 2 tiles: prefer government sources (Prompt 3b) for the forward view; tile 4 should state the growth sectors and their nature need as the source states it. Page 3 links (s2_link_<Theme_ID>) give the client's main disclosed exposure for each theme, with £ and % of total from my Calculations. Page 4 columns 1–3 are the top three themes by score (h1–h3): "today" = facts only; "gap" = not yet visible (absence check, list the sources searched); "opp" = EY view with rationale. Every percentage or total must equal a CALC value. Follow 04_Slide_Writing_Guide_and_Example; keep within character limits; copy its style, never its facts. Run the guide's self-check before replying. Return the two tables only.
```
*Paste table (a) into the Slide_Content paste area (cell J5, columns J:L); columns B and C look it up by key. Paste table (b) into Claims_Register from row 5. Save. Fix every cell that QA_Log or the red flags show.*

### Prompt 8 – Self-check
```
Step 8. Here is my QA_Log and any red rows from Slide_Content and Claims_Register: <paste>. For each Fail, tell me the cause and give corrected rows.
```
*Repeat until QA_Log is all Pass.*

### Prompt 9 – Fill the deck in PowerPoint (no code)

**9a – Prepare**
1. Save the workbook so the formulas recalculate.
2. Copy `Nature_Deck_Skeleton_EY.pptx` and name it `<Client>_Nature_Opportunities.pptx`.
3. Open the copy in PowerPoint (desktop or web).

**9b – Placeholders.** In the workbook's Deck_Tokens tab, copy A5:B down. In the Copilot pane inside PowerPoint, send:
```
This presentation contains placeholders in double curly braces, for example {{s1_title}} and {{r1_link}}. Replace every placeholder, including those in the speaker notes, with the matching Value from the table below. Replace only the placeholder text: keep all fonts, colours, shapes, positions and charts exactly as they are. Where a Value contains line breaks, keep them as separate lines. Do not rewrite, shorten or add to any Value, and do not change anything in square brackets [ ].
<paste the Placeholder | Value table>
```
*Fallback: Home → Replace (Ctrl+H), one placeholder at a time (about 25 minutes).*

**9c – Charts.** For each chart: right-click → Edit Data → click cell A1 of the data sheet → paste → close.
- Page 2 chart: Deck_Chart A5:C10 (Chart A)
- Page 3 left chart (score): A13:C18 (Chart B)
- Page 3 right chart (money): A21:C26 (Chart C)

**9d – Page 5.** The EY team writes page 5 by hand. Replace every [EY team] placeholder.

**Then check:**
- Search for `{{` (Home → Find); none should be left.
- Search for `[EY team`; none should be left.
- The speaker notes on pages 2–4 start with the talk track and then list `[Sources]`.
