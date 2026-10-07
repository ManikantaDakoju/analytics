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
Step 5. For all 15 SubTheme_IDs produce the Scoring (agent part) table: SubTheme_ID | C1 | C2 | C4 | Evidence_IDs | Rationale | Client exposure (as disclosed). Use the rubric scales exactly, apply the C2 cap, cite Evidence_IDs for every score, and label sector-based reasoning as "EY view". Where there is no evidence, say so and score C1 = 1.
```

### Prompt 6 – Self-check
```
Step 6. Run the QA checklist in the methodology file against everything you produced. Report each check as Pass or Fail, list any fixes, and give corrected rows for anything that failed.
```

*Now paste all tables into the master template and check that the QA_Log tab shows Pass. Copy the Scoring tab, sorted by Rank (columns Rank to Confidence plus Evidence_IDs and Client exposure).*

### Prompt 7 – Slide content
```
Step 7. Here is the final ranked Scoring table from the workbook:
<paste table>
Draft the 3 slides using the Slide specification and Slide Content Format. Use only these scores and the evidence already extracted. Titles 75 characters or fewer. Include speaker notes with [Sources] … [/Sources] for each slide.
```

### Prompt 8 – Build in PowerPoint (Copilot in PowerPoint, not the agent)
Open a new deck from the EY template first, then use Copilot in PowerPoint:
```
Using this presentation's EY template layouts, add three content slides with the text below. Keep my wording exactly. Put each [Title] in the slide title, the [Client line] under it, tables as native PowerPoint tables, and [Speaker notes] in the notes. Do not add images or change the theme.
<paste Step 7 output>
```
If Copilot changes the layout or wording, paste each block into the template placeholders yourself. It takes about 10 minutes and is the most reliable option.
