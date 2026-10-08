# Nature Opportunity Assessment – Copilot Playbook (v2)

How to repeat the LBG proof of concept for any client in any country covered by the EY NBSAP Nature Sizing workbook.

## 1. How the work is split

| Who | Does what | Why |
|---|---|---|
| **Copilot agent** | Reads client documents, extracts evidence and exposure, maps to the taxonomy, proposes C1, C2 and C4 scores, drafts slide text | Language tasks where AI is fast |
| **Master template (Excel)** | Holds the taxonomy and country NBSAP data; calculates C3, C5, weighted score, tier, rank and confidence; runs QA checks | Maths stays consistent and auditable across every client |
| **Deck skeleton + Copilot in PowerPoint** | The skeleton is the EY template with the 3 slides already laid out. Copilot in PowerPoint (or Find and Replace) swaps placeholders for the values in the workbook's Deck_Tokens tab | No code; same layout for every client |
| **build_deck.py** (out of date) | Builds the earlier layout only | Kept for reference; not used |
| **You** | Check the evidence, approve scores, sign off | Professional judgement and quality |

## 2. Files in this pack

| File | Use |
|---|---|
| `00_Playbook.md` | This guide |
| `01_Agent_Instructions.md` | Paste into the agent's **Instructions** field (about 4,000 characters; Copilot usually allows 8,000) |
| `02_Methodology_and_Specifications.md` | Save as .docx or .pdf and upload as agent **Knowledge** |
| `03_Run_Prompts.md` | The exact prompts for each run, in order |
| `04_Slide_Writing_Guide_and_Example` (.docx and .md) | Upload the .docx as agent **Knowledge**. Shows Copilot how to write every slide field: rules, limits, good and weak examples, and the full LBG example. |
| `Nature_Opportunity_Master_Template.xlsx` | Blank workbook for every client; contains the full taxonomy, all countries' NBSAP commitments and a Slide_Content tab |
| `Nature_Deck_Skeleton_EY.pptx` | EY template with the 3 slides laid out and placeholders, ready to fill for each client |
| `build_deck.py` | Out of date (earlier layout); kept for reference only |

## 3. One-time setup (about 20 minutes)

1. In Microsoft 365 Copilot, choose **Create agent** (or use Copilot Studio) and open the **Configure** tab. Menu names vary by version.
2. **Name:** Nature Opportunity Analyst. **Description:** Maps a client's public nature disclosures to the national NBSAP taxonomy and drafts a 3-slide summary.
3. **Instructions:** paste `01_Agent_Instructions.md`.
4. **Knowledge:** upload these files:
   - `02_Methodology_and_Specifications` (.docx or .pdf)
   - the master template
   - `04_Slide_Writing_Guide_and_Example.docx` (how to write each slide field)
5. **Capabilities:**
   - Turn web search **off**. You upload the sources, which keeps runs repeatable.
   - Turn on code interpreter if EY has it enabled; it helps with reading Excel files.
6. Test with the LBG files. You should get the same top four as the POC deck: Risk reduction and prevention, Land-use change and habitat protection, Water availability, Water quality.

## 4. Each new client (about 1–2 hours of analyst time)

| Step | You do | Prompt |
|---|---|---|
| 0 | Copy the master template as `<Client>_<Country>_Nature_Mapping.xlsx`, set Inputs (Client, Country_ID). Collect the client's latest public documents: annual or sustainability report, data sheet, financing framework, TNFD or nature report. | Prompt 0 |
| 1 | Paste the Sources table | Prompt 1 |
| 2 | Paste Evidence_Log rows. **Spot-check 5 quotes against the PDF.** | Prompt 2 (once per document) |
| 3 | Paste Client_Exposure rows. Check the figures match the source. | Prompt 3 |
| 4 | Paste NBSAP_Mapping (and NBSAP_Targets) | Prompt 4 |
| 5 | Paste C1, C2, C4, Evidence_IDs, Rationale, Exposure into the blue columns of Scoring. **Review each score.** | Prompt 5 |
| 6 | Fix anything flagged. Check the QA_Log tab is all Pass. | Prompt 6 |
| 7 | Copy the ranked Scoring table back to the agent; paste its Key / Value output into the Slide_Content tab; save in Excel | Prompt 7 |
| 8 | Copy the deck skeleton and open it in PowerPoint. Paste the Deck_Tokens table into Copilot in PowerPoint (or use Ctrl+H), then paste the Deck_Chart data into the page 3 chart (Edit Data). | Prompt 8 |

## 5. Is the mapping file UK-only?

**No.** There are two workbooks:

| Workbook | Scope |
|---|---|
| `LBG_UK_NBSAP_Mapping_v2.xlsx` | LBG-specific: holds LBG evidence and scores, and UK-only commitment values with UK-calibrated bands. Keep it as the worked example. |
| `Nature_Opportunity_Master_Template.xlsx` | **Any country** in the EY NBSAP workbook (currently GBR, CAN, QAT, SAU, ARE, FRA, ESP, DEU, IRL, PRT, ITA). Change `Inputs!B6` (Country_ID) and every NBSAP figure recalculates. |

What makes the master template reusable:
- **Comparable values:** NBSAP commitments for all countries are loaded, and values use the USD column, so countries are comparable.
- **No currency effect:** C5 (market opportunity) uses each sub-theme's **relative rank within the chosen country**, so the size of an economy or its currency doesn't skew scores. With UK data, the top four match the LBG POC; a few lower-ranked scores move by 0.1.
- **Taxonomy-consistent:** commitments are always assigned to the parent of their sub-market, so tagging errors in the source data don't change totals.
- **Country-neutral:** the agent, rubric and slide rules contain nothing UK-specific.

**To add a new country:**
1. Add the country to the EY NBSAP workbook (Countries, Market_Examples) as you already do.
2. Paste the new rows into the template's Countries and NBSAP_Commitments tabs.
3. Upload the country's NBSAP document so the agent can complete NBSAP_Targets.

**Caveats:**
- **Data depth varies by country.** Portugal has no NBSAP, and Qatar's is from 2015. Low NBSAP data means low C3 and C5 for every sub-theme, and the slides should say so.
- **NBSAP goal links are incomplete.** Only the UK Water goal is referenced in the EY workbook. Complete NBSAP_Targets for each country you run.
- **Sector lens.** The rubric suits banks and insurers. For a corporate client, C1 should draw on operations and supply chain rather than lending exposure. The prompts already allow this; mention the sector in Prompt 0.
- **Known issues in the EY workbook** (see the LBG workbook's Data_QA tab): USD double conversion in Market_Size, a DM02 formula error, and mismatched Market_ID tags. The master template sidesteps these, but the Power BI tool still shows them.

## 6. Getting output like the POC deck without code

- **Fixed layout:** `Nature_Deck_Skeleton_EY.pptx` holds the final layout on your EY template:
  - **Page 2:** the executive summary story (signal, rules, why the client), the top four priorities and a "So what" line.
  - **Page 3:** a priority bar chart beside the client's exposure tiles.
  - **Page 4:** the "How EY can assist" cards.

  Only text and chart data change between clients.
- **Calculated values:**
  - The **Deck_Tokens** tab works out every placeholder value with formulas: the top-four priorities, the Low list (using short names), and the national commitment count and value for card 1.
  - The **Deck_Chart** tab gives the ranked chart data, ready to paste.
  - All other text comes from Slide_Content.
- **What you do by hand:** paste the chart data (Edit Data). Optionally add yellow borders to nature-priority tiles and colour the rank chips by tier.
- **Currency:** card 1's automatic value is in US dollars, matching the EY sizing tool. Type a figure in local currency into c1_stat_text if you prefer, as in the LBG deck.
- **Never use a deck the agent generates itself.** A .pptx downloaded from Copilot chat (code interpreter or "create a presentation") is often a set of slide pictures that can't be edited. Always fill the skeleton in PowerPoint instead (Prompt 8).
- **If text overflows:** keep proof points to 8 words or fewer, tile labels to 6 words or fewer, and action titles to 75 characters or fewer.
- **If EY changes the template:** ask for the skeleton to be rebuilt on the new version. The placeholder names stay the same, so the workbook still works.

## 7. Tips for reliable Copilot runs

- **One step per message.** Long multi-step prompts produce shallow extraction.
- **Chunk long reports:** use 20–40 pages per prompt.
- **Always ask for exact headers** so tables paste straight into Excel. If a table arrives as text, ask: "Return that as a table with the exact headers."
- **Keep the agent away from maths.** If it starts calculating weighted scores, remind it: "Only C1, C2 and C4; the workbook calculates the rest."
- **Spot-check before scoring.** Check quotes and exposure figures first; most errors appear there.
- **Keep data inside EY.** Use EY-approved Copilot only for client and EY material.
