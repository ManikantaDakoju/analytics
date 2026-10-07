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

### Prompt 8 – Build the deck (no code needed)

You need:
- `Nature_Deck_Skeleton_EY.pptx`: the EY template with all three slides already laid out, and placeholders such as `{{s1_title}}` and `{{1_name}}`
- the **Deck_Tokens** tab of your client workbook: every placeholder with its value, calculated by formulas from Scoring and Slide_Content

**Step 8a – Prepare**
1. Save your client workbook in Excel, so the formulas recalculate.
2. Make a copy of `Nature_Deck_Skeleton_EY.pptx` named `<Client>_Nature_Priorities.pptx` and open it in PowerPoint.
3. In the workbook's **Deck_Tokens** tab, copy columns A and B from row 5 down.

**Step 8b – Ask Copilot in PowerPoint** (Copilot pane inside the open deck)
```
This presentation contains placeholders in double curly braces, for example {{s1_title}} and {{1_name}}.
Replace every placeholder with the matching Value from the table below. Replace only the placeholder text: keep all fonts, colours, shapes, positions and tables exactly as they are. Also replace placeholders in the speaker notes. If a Value is blank, delete the placeholder and leave the cell or box empty. Do not add, move, resize or restyle anything, and do not rewrite any Value.
<paste the Placeholder | Value table>
```
Then check:
- No `{{` is left anywhere (use Home → Find, and search for `{{`).
- On page 3, delete any empty table rows: select the row, then right-click → Delete Rows.
- Optional: colour the Tier cells for High rows yellow, to match the POC deck.

**Fallback if Copilot can't do the replacement:** use PowerPoint's own **Home → Replace (Ctrl+H)**.
- Copy each placeholder from column A and its value from column B, then click **Replace All**.
- Doing all of them takes about 20–25 minutes. The layout never changes, because you only replace text.
- Do slide 1 and slide 3 boxes first. For the page 3 table, you can click into each cell and paste.

**Optional – build_deck.py:** where Python is allowed (not on EY laptops at present), `build_deck.py` builds the same deck in one step. See the playbook.
