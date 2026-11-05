# Run prompts (v4) – copy into the Copilot agent, one at a time

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
*Paste into the blue columns E, F, H, P, Q and R of the Scoring tab.*

### Prompt 6 – Glossary and calculations
```
Step 6. (a) List every abbreviation and defined term you expect to use on the slides that is not already in the Glossary (I paste the current Glossary IDs below). For each: ID (ABB- or DEF-) | Term | Type | Definition / expansion | Source_ID | Reference (page/tab). The expansion must come from a source; if no source spells it out, say so and suggest wording that avoids the abbreviation.
(b) For every client percentage or total the slides need (e.g. a sector as % of total exposure), give a Calculations row: CALC_ID (CALC-51 …) | Description | Excel formula referencing Client_Exposure cells (I paste the Client_Exposure rows with their row numbers) | Unit | How it is calculated | Input trace IDs | Source_ID.
<paste Glossary column A and Client_Exposure rows with row numbers>
```
*Paste Glossary rows from row 60 and Calculations rows from row 30. Save, so the values calculate.*

### Prompt 7 – Slide text and claims
```
Step 7. Here are the workbook tables:
Theme_View (columns A–P): <paste>
Scoring sorted by Rank (columns A–L): <paste>
Calculations (columns A–C): <paste>
Deck_Tokens rows for s1_left_sub, s1_callout_stat, r1–r5 and h1–h3: <paste>
Write the text for pages 2–4 as two tables.
(a) Slide_Content: Key | Value | Claim_IDs, for exactly the keys in SRC-METH section 10, in that order.
(b) Claims_Register: Claim_ID | Slide | Slide_Content key | Claim | Claim type | Primary trace ID | Other trace IDs | Derivation / how checked, for every claim (C-001 …), plus "auto:" claims for the Excel-written text listed above.
Rules: page 3 links (s2_link_<Theme_ID>) give the client's main disclosed exposure for each theme, with £ and % of total from my Calculations. Page 4 columns 1–3 are the top three themes by score (h1–h3): "today" = facts only; "gap" = not yet visible (absence check, list the sources searched); "opp" = EY view with rationale. Every percentage or total must equal a CALC value. Follow 04_Slide_Writing_Guide_and_Example; keep within character limits; copy its style, never its facts. Run the guide's self-check before replying. Return the two tables only.
```
*Paste into Slide_Content (columns B–C) and Claims_Register. Save. Fix every cell that QA_Log or the red flags show.*

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
