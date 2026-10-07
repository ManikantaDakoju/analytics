# Run prompts – copy and paste into the Copilot agent, one at a time

Replace the text in <angle brackets>. Wait for each answer, check it, paste the tables into the master template, then send the next prompt.

---

### Prompt 0 – Start a new client
```
Start a new nature opportunity assessment.
CLIENT: <Lloyds Banking Group (LBG)>
COUNTRY_ID: <GBR>
SECTOR: <Banking and financial services>
AS_AT_DATE: <07/10/2026>
AUDIENCE: <Internal (EY partner)>
I have uploaded: <list the files>.
Confirm the inputs back to me and tell me if anything is missing. Do not start the analysis yet.
```

### Prompt 1 – Source inventory
```
Step 1. List every uploaded client document as a Sources table using the exact headers in the methodology file. Assign SRC-01, SRC-02 … in the order listed. Flag any document older than 24 months or that you could not read fully.
```

### Prompt 2 – Evidence extraction (repeat for each source)
```
Step 2 for <SRC-01>. Read the whole document. Produce Evidence_Log rows (start at <EV-001>) for every nature-related commitment, target, policy, exposure, financing activity, product, eligibility criterion, partnership, metric or disclosure, and for regulations or market drivers the document cites. Quotes must be copied exactly, 25 words or fewer, with page numbers as printed. Do not summarise the document; only produce the table. Tell me the last Evidence_ID you used.
```
Tip: for a long report, add "Pages <x–y> only" and run it in chunks.

### Prompt 3 – Portfolio exposure
```
Step 3. From <SRC-0x> (<tab or page of the exposure / concentration / TNFD priority sector table>), produce Client_Exposure rows exactly as disclosed: sector, sub-sector, exposure amount, currency, nature priority flag and % of total where given. Suggest Linked SubTheme_IDs for each row and label them "EY view". Do not convert currencies or estimate missing values.
```

### Prompt 4 – Mapping
```
Step 4. Map every Evidence_ID from Step 2 to the taxonomy. Produce NBSAP_Mapping rows with Evidence class, Primary SubTheme_ID, Primary Sub_Market_ID, Secondary SubTheme_ID, Mapping strength and a one-line rationale. Use XC only for group-level or cross-cutting items.
<If you uploaded the NBSAP document:> Also produce NBSAP_Targets rows for COUNTRY_ID: for each of the 15 SubTheme_IDs give the most relevant NBSAP goal/target, policy signal strength and page, or "None identified".
```

### Prompt 5 – Judgement scores
```
Step 5. For all 15 SubTheme_IDs produce the Scoring (agent part) table with these exact columns: SubTheme_ID | C1 | C2 | C4 | Evidence_IDs | Rationale | Client exposure (as disclosed) | Key evidence for slide (≤12 words) | Exposure for slide (≤4 words). Use the rubric scales exactly, apply the C2 cap, cite Evidence_IDs for every score, and label sector-based reasoning as "EY view". Where there is no evidence, say so, score C1 = 1 and leave the two slide columns blank.
```
*Paste C1, C2, C4, Evidence_IDs, Rationale, Client exposure, Key evidence and Exposure for slide into the blue columns E, F, H, P, Q, R, T, U of the Scoring tab.*

### Prompt 6 – Self-check
```
Step 6. Run the QA checklist in the methodology file against everything you produced. Report each check as Pass or Fail, list any fixes, and give corrected rows for anything that failed.
```

*Now check that the QA_Log tab shows Pass. Copy the Scoring tab sorted by Rank (Rank to Confidence columns).*

### Prompt 7 – Slide content (as a Slide_Content table)
```
Step 7. Here is the final ranked Scoring table from the workbook:
<paste table>
Produce the Slide_Content table with two columns, Key | Value, using exactly the keys listed in the Slide_Content tab of the master template (cover_title … s3_notes). Follow the guidance limits for each key. Use only these scores and the evidence already extracted. Cards 1–4 on slide 3 are the top four priorities (merge two water sub-themes into one card if both rank). For *_notes keys, list sources one per line. Return the table only.
```
*Paste the Key | Value table into the Slide_Content tab (column B), save the workbook in Excel.*

### Prompt 8 – Build the deck (choose one route)

**Route A – Same output as the POC deck (recommended). Copilot with code interpreter, or any Python.**
Upload `build_deck.py`, the EY template (.pptx) and the completed workbook to a Copilot chat or agent that can run Python (code interpreter or "Analyst"), then send:
```
Run the attached Python script build_deck.py exactly as written, with no changes, using:
--template <EY_Template.pptx> --workbook <Client_Country_Nature_Mapping.xlsx> --out <Client>_Nature_Priorities.pptx
If a library is missing, tell me which one; do not rewrite the script. Give me the output file to download.
```
If Copilot cannot run code, run it yourself, if EY allows Python on your laptop:
`pip install python-pptx openpyxl` then
`python build_deck.py --template EY_Template.pptx --workbook <workbook>.xlsx --out <Client>_Nature_Priorities.pptx`

**Route B – Copilot in PowerPoint (no code).** Open a new deck from the EY template, then:
```
Using this presentation's EY template, add three content slides from the text below. Keep my wording exactly. Slide 1: three stat boxes on the left and the summary bullets on the right. Slide 2: a native table with columns #, Sub-theme, NBSAP $ | no., Client exposure, Score, Tier, Key client evidence, then a Low-priority line. Slide 3: four cards side by side (evidence, support, output), then the cross-cutting line and next steps. Put sources in the speaker notes. Do not add images or change the theme.
<paste Slide_Content table and the ranked Scoring rows>
```
Route B gives a similar structure, but layouts vary from run to run; adjust spacing by hand.

**Route C – Manual.** Paste each Slide_Content value into the template placeholders (about 15 minutes).
