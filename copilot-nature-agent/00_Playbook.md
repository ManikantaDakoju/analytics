# Nature Opportunity Assessment – Copilot Playbook (v4.4)

How to produce the client pack (cover + 4 pages) for any client, in any country covered by the EY NBSAP Nature Sizing workbook, using Microsoft 365 Copilot, Excel and PowerPoint only. No code, and no tools beyond Microsoft 365.

## 1. What you get

| Page | Question it answers | Who writes it |
|---|---|---|
| Cover | — | Workbook (title and date from Slide_Content) |
| 2 Context | What is an NBSAP and what did we measure (context band); where is national nature money going; where is the country heading? | Copilot writes the words; Excel writes the context band's first two blocks, the numbers and Chart A |
| 3 Context × client | How does that money meet the client's book, theme by theme? | Copilot writes the client links; Excel calculates scores, values, ranks and Charts B and C |
| 4 Client | For the top three themes: what the client does today, what is not yet visible, and the opportunity | Copilot (facts, absence checks and EY views, each traced) |
| 5 EY | How EY can support | **The EY team writes it by hand. The agent never drafts it.** |

Every value on pages 2–4 is traced through four links:

1. The slide text
2. A Claim_ID in the Claims_Register
3. A trace ID: evidence (EV-), NBSAP commitment (NB-), calculation (CALC-), score (SC-), glossary (DEF-/ABB-) or source (SRC-)
4. The source document, with its printed page and PDF page

The speaker notes list the sources for every claim automatically, and QA_Log fails if anything is untraced.

## 2. How the work is split

| Who | Does what |
|---|---|
| **Copilot agent** | Reads the client documents and extracts verbatim evidence with pages. Maps it to the taxonomy, proposes C1, C2 and C4, and writes the glossary entries, slide text and claims. |
| **Master template (Excel)** | Holds the taxonomy and all countries' NBSAP commitments. Calculates C3, C5, scores, theme roll-up, shares, percentages, chart data and every placeholder value. Runs 22 QA and traceability checks. |
| **Deck skeleton + Copilot in PowerPoint** | The EY template with the layout fixed and `{{placeholders}}`. Copilot in PowerPoint (or Ctrl+H) replaces the placeholders; you paste the chart data. |
| **You (and the EY team)** | Check evidence and scores, approve EY views, and confirm absence checks. Write page 5, then sign off (QRM). |

## 3. Files in this pack

| File | Use |
|---|---|
| `00_Playbook.md` | This guide |
| `01_Agent_Instructions.md` | Paste into the agent's **Instructions** field (about 5,000 characters) |
| `02_Methodology_and_Specifications` (.docx/.md) | Agent **Knowledge**. This is SRC-METH, the method the workbook cites. |
| `03_Run_Prompts.md` | Prompts 0–9 plus 3b (national growth priorities), in order |
| `04_Slide_Writing_Guide_and_Example` (.docx/.md) | Agent **Knowledge**: rules, field guide, and the full LBG example (slide text and all 76 claims) |
| `Nature_Opportunity_Master_Template.xlsx` | Blank workbook for every client: all countries, taxonomy, calculations, glossary, claims register and QA |
| `Nature_Deck_Skeleton_EY.pptx` | EY template, cover + 4 pages, with placeholders and three charts. Page 5 has [EY team] placeholders only. |
| `Nature_Opportunity_Assessment_Methodology.docx` | Plain-English methodology for partners and reviewers |
| `build_deck.py` | Retired. Do not use. |
| `tools/nature_v41/` (repo only) | Scripts EY used to build and test the toolkit. Not needed to run it. |

The LBG worked example is in `lbg-nature-poc/`:
- `LBG_UK_Nature_Mapping_v4.4.xlsx`: the master template filled in for LBG, with every QA check passing
- `LBG_Nature_Opportunities_v4.4.pptx`: the skeleton filled from that workbook

## 4. One-time setup (about 20 minutes)

1. In Microsoft 365 Copilot, choose **Create agent** (or use Copilot Studio) and open the **Configure** tab.
2. **Name:** Nature Opportunity Analyst.
3. **Instructions:** paste `01_Agent_Instructions.md`.
4. **Knowledge:** upload `02_Methodology_and_Specifications.docx` and `04_Slide_Writing_Guide_and_Example.docx`. Do **not** upload client documents or the master template as Knowledge: client files go in each chat, and the workbook stays with you.
5. **Capabilities:** web search **off**.
6. **Test** with the LBG files. Your slide text should be close to the LBG example, and QA_Log should be all Pass.

## 5. Each new client (about 2–3 hours of analyst time)

| Step | You do | Prompt |
|---|---|---|
| 0 | Copy the master template as `<Client>_<Country>_Nature_Mapping.xlsx`. Fill Inputs: client, short name, Country_ID, country label, sector, as-at date. Collect the client's latest public documents: **annual and/or sustainability report (core)**, data sheet, financing framework, nature/TNFD/climate report. If the annual / sustainability report can't be used, record why and keep the page 4 caveat; QA shows a Warning until it is reviewed. | 0 |
| 1 | Paste Sources from row 7 | 1 |
| 2 | Paste Evidence_Log, one source at a time. **Spot-check 5 quotes against the PDF page.** | 2 |
| 3 | Paste Client_Exposure (including the group total) and the matching evidence rows | 3 |
| 3b | Attach the country's official growth strategies (e.g. Industrial Strategy, AI Opportunities Action Plan, Environment Agency water framework) and, optionally, the ENCORE Insights Country Dashboard data (countries.encorenature.org). Paste the evidence rows and the Growth_Sectors rows. Do this once per country and reuse it for later clients. | 3b |
| 4 | Paste NBSAP_Mapping. Approve any classification review in NBSAP_Commitments columns N and O. | 4 |
| 5 | Paste the whole table at Scoring!W5 (any row order). **Review each score.** | 5 |
| 6 | Paste Glossary rows (from row 60) and Calculations rows (from row 30). Save. | 6 |
| 7 | Paste back Theme_View, Scoring, Calculations, Growth_Sectors and selected Deck_Tokens. Paste the agent's Slide_Content at Slide_Content!J5 (any order) and the claims into Claims_Register. | 7 |
| 8 | Fix anything QA_Log or the red cells flag | 8 |
| 9 | Fill the skeleton in PowerPoint: placeholders via Copilot in PowerPoint (or Ctrl+H), then charts via Edit Data | 9 |
| 10 | The EY team writes page 5. Book QRM. | — |

## 6. Traceability: what "every detail traced" means in practice

- **Quotes:** verbatim, 25 words or fewer, with both printed and PDF page. Sources records how each document numbers its pages.
- **Numbers:** totals, shares and percentages are Excel formulas (Calculations, Theme_View), never typed. The Claims_Register **Value check** confirms each calculated figure on a slide equals its formula.
- **Definitions and abbreviations:** every abbreviation on a slide has a Glossary entry with a sourced expansion. If no source spells one out, the slide rewords it. In the LBG pack, "ELMs" was dropped for this reason.
- **Judgements:** EY views cite their supporting evidence and state a rationale.
- **Absences:** "not yet visible" claims list the documents searched, and the footnote names the documents not reviewed.
- **Speaker notes:** built automatically (talk track, then `[Sources]` with one line per claim).

## 7. Is it country-neutral?

Yes. Change Inputs Country_ID and the label, and everything recalculates. Specifically:
- **C5** uses each sub-theme's share of the country's total, so currency and economy size don't distort scores.
- **Currency:** amounts show in local currency when all of a country's commitments use one currency (e.g. GBP, EUR, CAD); otherwise in USD.

**Caveats:**
- **Data depth varies.** Portugal has no NBSAP and Qatar's dates from 2015. Low data gives low C3 and C5 for every sub-theme, and the slides should say so.
- **Theme names** are toolkit labels derived from Market_ID prefixes. Confirm them with the EY workbook owner.
- **Known issues in the EY workbook:** see Data_QA in the v2 and v3 LBG workbooks (USD double conversion in Market_Size, a DM02 formula error, Market_ID tag mismatches, Market_Examples sources). The template avoids them by summing Market_Examples directly.

## 8. Tips for reliable Copilot runs

- **One step per message.** Chunk long reports by PDF page range.
- **Ask for exact headers** so tables paste straight in. If Copilot returns prose, ask: "Return that as a table with the exact headers."
- **Keep Copilot away from maths.** If it starts calculating shares or totals, remind it that the workbook does that.
- **Don't let the agent create the deck.** It must never build a .pptx itself: files generated in chat are often pictures of slides.
- **Use EY-approved tools only** for client and EY material.
