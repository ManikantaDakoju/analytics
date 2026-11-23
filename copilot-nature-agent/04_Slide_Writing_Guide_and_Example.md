# Slide Writing Guide and Worked Example (v5.1)

Knowledge file for the Copilot agent. It shows how to write every Slide_Content value for pages 3–5, and the Claims_Register rows behind them. Use it with SRC-METH (02_Methodology_and_Specifications). There is no Why EY page.

## 1. Rules

1. **Copy the style, never the facts.** The worked example is Lloyds Banking Group (UK). Never reuse its names, figures or wording for another client.
2. **Trace everything.** Every value except structural keys lists Claim_IDs, and every Claim_ID has a Claims_Register row whose primary trace resolves (EV-, NB-, CALC-, SC-, DEF-, ABB-, SRC-). No trace, no text.
3. **Numbers come from the workbook.** Percentages, totals, theme values and scores must equal a CALC- or SC- value. Exposure figures are quoted (EV-) and converted only for units (£365,599m → £365.6bn), with the conversion written in the claim derivation.
4. **Quotation marks mean exact words.** Only put words in quotation marks if the source uses exactly those words. Otherwise paraphrase without quotation marks and put the exact quote in the claim derivation.
5. **Action titles state the finding**, in 68 characters or fewer. Write "LBG meets that money mainly through flood risk in its mortgage book", not "Exposure analysis".
6. **Write words in full.** Spell out wherever space allows: biodiversity net gain (not BNG), key performance indicators (not KPIs), the full programme name (not HARP), "such as" (not e.g.), "about" (not c.), "including" (not incl.), "hectares" (not ha), months in full, "page" (not p.). Keep an abbreviation only for names or where the limit requires it (client short name, EY, NBSAP, ENCORE, country label, regulator codes such as PRA SS5/25).
7. **Every abbreviation you keep needs an ABB- glossary entry with key text** (e.g. "UK = United Kingdom"), whose expansion comes from a source. Excel lists it in the abbreviation key at the foot of the page. If no source spells it out (e.g. "ELMs" in the LBG framework), reword instead of abbreviating.
8. **Forward view from government sources.** Page 2 tiles about where the country is heading should cite national strategies (Prompt 3b), not the client’s own commentary, wherever possible.
9. **Facts, gaps and views are separate.** Page 4 "today" is facts only. "Gap" (shown on the slide as "Questions to explore") is an absence check that lists the sources searched (never "LBG fails to…"). "Opportunity" is an EY view with a rationale.
10. **Say "not yet visible"**, not "missing" or "lacking". The footnote says which documents were reviewed and which were not (e.g. the annual report).
11. **Write money and units consistently:** £12.1bn, £120m, 9.3%, about £41bn (rounded), 238,494 hectares. Shares of the client’s book take one decimal place everywhere (56.7%, not 57%). Use the country's currency symbol.
12. **Use UK English and a neutral tone.** Never name individuals, never use EY confidential data, never mention fees.
13. **Page 4 exposure panel:** s3_exp_stat is one figure that equals a CALC value (e.g. the nature-priority share of the book); s3_exp_text says what it is and how the largest sector is classified, using the client’s own definitions.
14. **Never draft a Why EY or EY offer page.** Page 2 (what the agent does) is fixed text in the skeleton.
15. **Check opportunities against the client’s own framework.** If the client already offers it, reword it as scale, connect or report, so the client cannot say "we already do that".
16. **Return only the two tables** (Slide_Content; Claims_Register), with no blank keys.

## 2. Field guide (limit = maximum characters)

| Key | What to write | Limit | Good (LBG example) | Weak | Why weak |
|---|---|---|---|---|---|
| s1_title | Action title: the finding about the national context, ≤68 characters | 68 | UK nature investment and future demand centre on water and flood | UK nature context | A topic label, not a finding. |
| s1_callout_text | Words after the automatic % (e.g. "goes to water and flood together (c.£41bn)"); cite CALC-TOP2-V | 75 | of identified commitments are directed to water and flood (about £41bn) | is a lot of money | No subject; no traced value. |
| s1_tile1_stat | Tile 1 figure, e.g. £725bn | 16 | £725bn | Huge | Not a figure. |
| s1_tile1_text | What the figure is, with scope | 85 | UK 10-year infrastructure pipeline, including £7.9bn for flood management (2026–36) | infrastructure spending is going up | No scope, no source figure. |
| s1_tile3_text | What the figure is, with scope | 85 | Projected public water shortfall by 2055 without action | water is running out | Alarmist and unsourced; give the figure, date and publisher. |
| s1_tile4_text | Text; label EY view in the claim | 85 | Housing, net zero, food and data centres all require water | Growth needs water (EY view) | Use the government’s own statement where one exists; name the sectors. |
| s1_rules | Regulations with dates, ≤1 line | 140 | 10% biodiversity net gain is mandatory, while PRA SS5/25 raises expectations for banks on physical and transition climate risk. | Regulation is getting stricter | No rule named, no date. |
| s1_sowhat | One sentence: what the context means for banks / the client sector | 200 | Growth in water-dependent sectors will increase the need for resilient infrastructure, creating a growing role for financing water and flood resilience. | Banks should look at nature | Not specific to the evidence. |
| s2_title | Action title: the priority themes and their share of national commitments, ≤68 characters | 68 | Three priority themes take 81% of UK nature commitments | Client exposure | A label, not the finding. |
| s2_link_W | Main client link for the Water theme: sector, £ and % of book | 160 | Energy and water supply £12.1bn (1.9%); direct financing link through £120m for the Haweswater Aqueduct Resilience Programme. | Utilities ~£12bn | Approximate; no % of book; informal sector name. |
| s2_link_DM | Main client link for Flood, coastal and adaptation | 160 | Mortgages £365.6bn (56.7%) and property £26.3bn (4.1%): a potential channel for flood risk. The flood-exposed share of the mortgage book is not disclosed. | Lots of mortgages exposed to floods (est. £100bn) | Estimated; never estimate. Quote and calculate. |
| s2_sowhat | One sentence: the gap between where the money goes and the client book | 220 | 69% of identified UK commitments target water and flood. For LBG, the link goes beyond its 1.9% direct energy and water exposure to flood risk across the £365.6bn mortgage book. | The table shows the themes and exposures. | Describes the slide instead of the insight. |
| s3_title | Action title: the nature-priority share of the client’s book (not its largest sector), ≤68 characters | 68 | Nature-priority sectors make up 9.3% (£60.0bn) of LBG’s book | LBG today | A label; no finding, no traced figure. |
| s3_col1_today | Theme ranked 1: what the client does today (facts only), ≤150 characters | 240 | Underwrote £238.5m, alongside NatWest, for Cleve Hill, including coastal flood-defence measures. Flood doors and barriers are eligible under LBG’s sustainable financing framework. | LBG is doing lots on flooding | No traced facts. |
| s3_col1_gap | Theme ranked 1: not yet visible in the documents reviewed (absence check), ≤120 characters | 170 | What proportion of the £365.6bn mortgage book is exposed to flood risk? How much lending currently supports property-level flood resilience? | LBG has failed to disclose flood risk | Critical tone; absence must be "not yet visible" with sources searched. |
| s3_col1_opp | Theme ranked 1: opportunity (EY view), ≤110 characters | 170 | Scale resilience and remediation finance for homes and property, including flood-resilience retrofits and subsidence repair. | Big opportunity | Not specific; no rationale. |
| s3_sowhat | One sentence: the next step for the client | 220 | LBG has enabling frameworks, partnerships and flagship transactions in place. For water and flood, the next step is to set portfolio targets, scale resilience finance and strengthen exposure data. | EY can help with many things | Not the client's next step; page 5 covers EY. |
| s1_ctx_why | Context band, third block: why it matters for the client’s sector, quoted (e.g. finance gap, role of private finance) | 150 | 51% of UK economic output is highly dependent on nature, while LBG estimates a £56bn UK nature finance gap. | Nature is important for banks | No evidence, no figure, no source. |
| s3_exp_text | Words after the headline figure: what it is, and how the largest sector is classified (facts only) | 90 | Exposure in sectors identified by LBG as nature priorities | of the book is green | Not the client’s own definition; vague; mixes up nature-priority and green finance. |

Structural keys (no Claim_IDs needed): cover_title, cover_date, client_name, s1_section, s1_heading_r, s1_rules_lead, s1_footnote, s1_talk, s2_section, s2_talk, s3_section, s3_footnote, s3_talk.

Excel writes these texts itself (do not write them): s1_ctx_what, s1_ctx_measured, s1_left_head, s1_left_sub, s1_callout_stat, the page 3 rows, h1–h3, the charts and the abbreviation keys (s1_key, s2_key, s3_key).

## 3. Worked example: Lloyds Banking Group (UK), Slide_Content

| Key | Value | Claim_IDs | Limit | Length |
|---|---|---|---|---|
| cover_title | Nature opportunities for Lloyds Banking Group |  | 60 | 45 |
| cover_date | For discussion – 10 December 2026 |  | 50 | 33 |
| client_name | Lloyds Banking Group |  | 40 | 20 |
| client_short | LBG | C-001 | 12 | 3 |
| s1_title | UK nature investment and future demand centre on water and flood | C-002, C-010, C-011 | 68 | 64 |
| s1_section | The UK context |  | 30 | 14 |
| s1_callout_text | of identified commitments are directed to water and flood (about £41bn) | C-012, C-013 | 75 | 71 |
| s1_heading_r | Where the UK is heading |  | 35 | 23 |
| s1_sub_r | Growth, water scarcity and regulation are increasing the need for investment | C-020 | 80 | 76 |
| s1_tile1_stat | £725bn | C-021 | 16 | 6 |
| s1_tile1_text | UK 10-year infrastructure pipeline, including £7.9bn for flood management (2026–36) | C-021, C-022 | 85 | 83 |
| s1_tile2_stat | 25m+ | C-023 | 16 | 4 |
| s1_tile2_text | people under water-use restrictions during the 2026 drought; reservoirs fell to 69% | C-023, C-024 | 85 | 83 |
| s1_tile3_stat | 5bn litres/day | C-025 | 16 | 14 |
| s1_tile3_text | Projected public water shortfall by 2055 without action | C-025 | 85 | 55 |
| s1_tile4_stat | Growth | C-027 | 16 | 6 |
| s1_tile4_text | Housing, net zero, food and data centres all require water | C-027 | 85 | 58 |
| s1_rules_lead | Regulatory expectations are also increasing |  | 50 | 43 |
| s1_rules | 10% biodiversity net gain is mandatory, while PRA SS5/25 raises expectations for banks on physical and transition climate risk. | C-028, C-030, C-031, C-032 | 140 | 127 |
| s1_sowhat | Growth in water-dependent sectors will increase the need for resilient infrastructure, creating a growing role for financing water and flood resilience. | C-034, C-119 | 200 | 152 |
| s1_footnote | Sources: EY NBSAP Nature Sizing workbook (indicative); Modern Industrial Strategy (2025) page 115; Environment Agency, National Framework for Water Resources 2025, pages 3, 6; Global Canopy, ENCORE Insights Country Dashboard (2026); LBG Built to Last (2026) pages 7, 10, 11; LBG Working with nature (2024) pages 7, 13. |  | 330 | 318 |
| s1_talk | Quick context first: every country has a national nature plan; in the UK it is the Environmental Improvement Plan 2025, and EY tracks the funded commitments behind it. Of about £60bn, 69% goes to water and flood. Then the direction of travel: the Industrial Strategy backs eight sectors, 32% of the economy, and calls nature degradation a risk to growth; it plans new reservoirs so development is not held back by water. The Environment Agency goes further: growth cannot happen without water. Close: water and flood resilience is a growth market for banks. |  | 1000 | 557 |
| s2_title | Three priority themes take 81% of UK nature commitments | C-161, C-160, C-002 | 68 | 55 |
| s2_section | Translating UK investment priorities to LBG’s portfolio |  | 60 | 55 |
| s2_link_W | Energy and water supply £12.1bn (1.9%); direct financing link through £120m for the Haweswater Aqueduct Resilience Programme. | C-047, C-048, C-049 | 160 | 125 |
| s2_link_AFF | Agriculture, forestry and fishing £6.8bn (1.1%); direct customer link through LBG’s soil partnership with 685 farms. | C-051, C-052, C-053, C-054 | 160 | 116 |
| s2_link_UD | Construction £5.9bn (0.9%), including £2.3bn housebuilders; direct link through development and land-use pressures. | C-055, C-056, C-057, C-162 | 160 | 115 |
| s2_link_DM | Mortgages £365.6bn (56.7%) and property £26.3bn (4.1%): a potential channel for flood risk. The flood-exposed share of the mortgage book is not disclosed. | C-041, C-042, C-043, C-044, C-045, C-046 | 160 | 154 |
| s2_link_P | Manufacturing £13.1bn (2.0%); indirect link through pollution, waste and resource-use pressures. | C-058, C-059, C-060 | 160 | 96 |
| s2_sowhat | 69% of identified UK commitments target water and flood. For LBG, the link goes beyond its 1.9% direct energy and water exposure to flood risk across the £365.6bn mortgage book. | C-010, C-048, C-061, C-041, C-046 | 220 | 177 |
| s2_talk | Same five themes, now set against LBG. The title says it: the top three themes take 81% of identified UK commitments. Read each row across: LBG’s relevance score, the UK money, and where LBG’s book touches it. A tick in the UK priority column means the theme takes at least 10% of UK commitments; the tag under a theme name means LBG itself names it as a priority. Water and flood lead, but LBG’s direct energy and water lending is small (1.9% of the book); the bigger link is indirect, through flood risk across the £365.6bn mortgage book. The strip at the bottom covers the other two themes: urban development, where mandatory biodiversity net gain creates demand for finance from housebuilders, and pollution and waste, which takes 18% of UK commitments but has only a weak link in LBG’s book. |  | 1000 | 796 |
| s3_title | Nature-priority sectors make up 9.3% (£60.0bn) of LBG’s book | C-155, C-147, C-154, C-001 | 68 | 60 |
| s3_section | Portfolio exposure, current actions and areas to explore |  | 60 | 56 |
| s3_col1_today | Underwrote £238.5m, alongside NatWest, for Cleve Hill, including coastal flood-defence measures. Flood doors and barriers are eligible under LBG’s sustainable financing framework. | C-072, C-073, C-074 | 240 | 179 |
| s3_col1_gap | What proportion of the £365.6bn mortgage book is exposed to flood risk? How much lending currently supports property-level flood resilience? | C-045, C-041, C-076 | 170 | 140 |
| s3_col1_opp | Scale resilience and remediation finance for homes and property, including flood-resilience retrofits and subsidence repair. | C-077 | 170 | 124 |
| s3_col2_today | Provided £120m of debt to the approximately £3bn Haweswater Aqueduct Resilience Programme, supporting water supply for around 2.5 million people. Wastewater treatment, reservoirs and wetlands are also eligible. | C-049, C-078, C-079, C-080, C-081, C-082 | 240 | 210 |
| s3_col2_gap | What water-specific lending targets and volumes are in place? How much of the £12.1bn energy and water supply exposure relates specifically to water? | C-083, C-047 | 170 | 149 |
| s3_col2_opp | Scale finance for drought resilience, storm-overflow reduction and water efficiency, including solutions for water-dependent growth sectors such as data centres. | C-084 | 170 | 161 |
| s3_col3_today | Ten-year Soil Association Exchange partnership covering 685 farms and 238,494 hectares. Regenerative agriculture and grant-supported projects are eligible; sustainability-linked loans already use borrower indicators. | C-054, C-053, C-087, C-085, C-086, C-168 | 240 | 216 |
| s3_col3_gap | Which nature-related metrics could complement the existing emissions target across the £6.8bn agriculture portfolio? | C-088, C-051, C-089 | 170 | 116 |
| s3_col3_opp | Link Soil Association Exchange farm scorecards to LBG’s existing sustainability-linked loans, and report the lending volumes. | C-090 | 170 | 125 |
| s3_sowhat | LBG has enabling frameworks, partnerships and flagship transactions in place. For water and flood, the next step is to set portfolio targets, scale resilience finance and strengthen exposure data. | C-092, C-070 | 220 | 196 |
| s3_footnote | Sources: LBG Working with nature (2024) page 4; Built to Last (2026) pages 14–17; Sustainability Data Sheet 2025, tabs 18 and 20; Sustainable Financing Framework 2025 pages 11–15, 24. "Areas to explore" = not found in these four documents; to be checked against the Annual and Sustainability Reports 2025. |  | 330 | 305 |
| s3_talk | Start with LBG’s own classification: 9.3% of its book, £60.0bn, sits in sectors it calls nature priorities. Mortgages are 56.7% of the book, but LBG classes them as 0% nature priority because homeowners’ direct influence on nature is low; they matter here for physical risk such as flooding. Then credit LBG: Cleve Hill, the Haweswater Aqueduct Resilience Programme, the Soil Association partnership and a financing framework that already covers water, flood resilience, regenerative farming and sustainability-linked loans. Then the areas to explore, which are things not found in the public documents, not criticisms. Then the opportunities. To make this LBG-specific, the next steps are LBG’s own lending data by sector, region and product, then property locations against flood and drought maps. |  | 1000 | 799 |
| s1_ctx_why | 51% of UK economic output is highly dependent on nature, while LBG estimates a £56bn UK nature finance gap. | C-120, C-001, C-117 | 150 | 107 |
| s2_other1_head | Urban development | C-105 | 40 | 17 |
| s2_other1_text | £5.9bn construction lending (a nature-priority sector for LBG) and mandatory biodiversity net gain point to net gain finance for housebuilders. | C-055, C-155, C-028, C-163 | 150 | 143 |
| s2_other2_head | Pollution and waste | C-105 | 40 | 19 |
| s2_other2_text | 18% of UK commitments (£10.8bn) but a weak link in LBG’s book: scope to grow waste and circular-economy finance under its existing framework. | C-164, C-165, C-166, C-002 | 150 | 141 |
| s2_lead | Water and flood represent 69% of identified UK nature commitments; the table maps these and the other nature themes to LBG’s portfolio. | C-010, C-002, C-001 | 160 | 135 |
| s3_exp_note | LBG classes mortgages as 0% nature priority, although they carry physical risks such as flooding. | C-156, C-001 | 130 | 97 |
| s3_exp_stat | 9.3% | £60.0bn | C-150, C-154 | 16 | 14 |
| s3_exp_sub | Share of £645.1bn total exposure by sector, as at 31 December 2025 | C-151, C-152, C-153 | 95 | 66 |
| s3_exp_text | Exposure in sectors identified by LBG as nature priorities | C-155, C-001 | 90 | 58 |

## 4. Worked example: Claims_Register (all rows)

| Claim_ID | Slide | Key | Claim | Type | Primary trace | Other traces | Derivation / how checked |
|---|---|---|---|---|---|---|---|
| C-001 | s2 | client_short | LBG | Abbreviation | ABB-LBG |  | Expansion in Glossary |
| C-002 | s1 | s1_title | UK | Abbreviation | ABB-UK |  |  |
| C-010 | s1 | s1_title | 69% (water and flood take most UK nature money) | Calculated | CALC-TOP2-S | CALC-S-DM, CALC-S-W | Top two themes by value = Flood, coastal and adaptation (38%) + Water (31%); after classification review (NB-M01, NB-M09, NB-M120, NB-M121) |
| C-011 | s1 | s1_title | growth will add more | EY view | EV-080 | EV-078, EV-079, EV-081, EV-061 | Government says growth needs water (EV-080) and projects a shortfall (EV-081); infrastructure pipeline (EV-061) |
| C-012 | s1 | s1_callout_text | £41bn | Calculated | CALC-TOP2-V | CALC-V-DM, CALC-V-W | £23.0bn + £18.3bn = £41.3bn, shown as about £41bn |
| C-013 | s1 | s1_callout_text | water and flood | Definition | DEF-TH-DM | DEF-TH-W | "flood" = theme Flood, coastal and adaptation (Disaster management and adaptation) |
| C-020 | s1 | s1_sub_r | Growth, water scarcity and regulation are increasing the need for investment | EY view | EV-080 | EV-019, EV-081, EV-070 | Summary of the four tiles and the rules line |
| C-021 | s1 | s1_tile1_stat | £725bn UK 10-year infrastructure pipeline | Quoted fact | EV-061 |  | UK Infrastructure: A 10-Year Strategy, as cited by LBG |
| C-022 | s1 | s1_tile1_text | £7.9bn for flood management (2026–36) | Quoted fact | EV-020 |  |  |
| C-023 | s1 | s1_tile2_stat | 25m+ people under water-use restrictions in the 2026 drought | Quoted fact | EV-062 | EV-019 | "more than 25 million people" (UK Government 2026, as cited by LBG) |
| C-024 | s1 | s1_tile2_text | reservoirs fell to 69% | Quoted fact | EV-019 |  |  |
| C-025 | s1 | s1_tile3_stat | 5bn litres a day public water shortfall by 2055 without action | Quoted fact | EV-081 |  | "5 billion litre a day shortfall by 2055 for public water supplies" (without continued action) |
| C-027 | s1 | s1_tile4_text | Housing, net zero, food and data centres all require water | Quoted fact | EV-079 | EV-078 | Paraphrase, no quotation marks: sectors named in EV-078 and EV-079; source wording "All of these require water" (EV-079) |
| C-028 | s1 | s1_rules | 10% biodiversity net gain is mandatory | Quoted fact | EV-005 |  |  |
| C-030 | s1 | s1_rules | PRA SS5/25 raises expectations for banks on physical and transition climate risk | Quoted fact | EV-070 | EV-073 | Source: "raising expectations for how UK banks and insurers identify and manage physical climate and transition risks" (corrected after the 19 November review: not physical risk only) |
| C-031 | s1 | s1_rules | PRA | Abbreviation | ABB-PRA |  |  |
| C-032 | s1 | s1_rules | SS5/25 | Abbreviation | ABB-SS525 |  |  |
| C-034 | s1 | s1_sowhat | Growth in water-dependent sectors will increase the need for resilient infrastructure | EY view | EV-080 | EV-081, EV-095, EV-079 | Government: growth cannot happen without water (EV-080); projected shortfall (EV-081); developments not held back by water (EV-095) |
| C-041 | s2 | s2_link_DM | £365.6bn mortgages | Quoted fact | EV-040 |  | £365,599m ÷ 1,000, 1 d.p. |
| C-042 | s2 | s2_link_DM | 56.7% | Calculated | CALC-52 |  |  |
| C-043 | s2 | s2_link_DM | property £26.3bn | Quoted fact | EV-068 |  | £26,270m ÷ 1,000, 1 d.p. |
| C-044 | s2 | s2_link_DM | 4.1% | Calculated | CALC-53 |  |  |
| C-045 | s2 | s2_link_DM | Flood exposure of the book not disclosed | Absence check | SRC-04 | EV-069 | SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); tab 18 reports climate (energy) and nature-priority flags only |
| C-046 | s2 | s2_link_DM | a potential channel for flood risk | EY view | EV-040 | EV-058, EV-066 | EY view: physical flood risk to mortgaged homes and property; flood exposure of the book not disclosed (C-045) |
| C-047 | s2 | s2_link_W | Energy and water supply £12.1bn | Quoted fact | EV-038 |  | £12,084m ÷ 1,000, 1 d.p. |
| C-048 | s2 | s2_link_W | 1.9% | Calculated | CALC-54 |  | Not the 1.7% nature-priority share shown in tab 18; this is total sector exposure ÷ Group total |
| C-049 | s2 | s2_link_W | £120m to the Haweswater Aqueduct Resilience Programme | Quoted fact | EV-023 | EV-064, ABB-HARP | Programme name spelled out (Glossary ABB-HARP, Built to Last PDF page 14) |
| C-051 | s2 | s2_link_AFF | Agriculture, forestry and fishing £6.8bn | Quoted fact | EV-075 |  | £6,776m ÷ 1,000, 1 d.p. |
| C-052 | s2 | s2_link_AFF | 1.1% | Calculated | CALC-55 |  |  |
| C-053 | s2 | s2_link_AFF | 685 farms | Quoted fact | EV-002 |  |  |
| C-054 | s2 | s2_link_AFF | LBG’s soil partnership (Soil Association Exchange, ten-year) | Quoted fact | EV-001 |  |  |
| C-055 | s2 | s2_link_UD | Construction £5.9bn | Quoted fact | EV-039 |  | £5,850m ÷ 1,000, 1 d.p. |
| C-056 | s2 | s2_link_UD | 0.9% | Calculated | CALC-57 |  |  |
| C-057 | s2 | s2_link_UD | £2.3bn housebuilders | Quoted fact | EV-076 |  | £2,262m ÷ 1,000, 1 d.p. |
| C-058 | s2 | s2_link_P | Manufacturing £13.1bn | Quoted fact | EV-045 |  | £13,095m ÷ 1,000, 1 d.p. |
| C-059 | s2 | s2_link_P | 2.0% | Calculated | CALC-56 |  |  |
| C-060 | s2 | s2_link_P | indirect link through pollution, waste and resource-use pressures | EY view | EV-045 | EV-059, EV-060 | Manufacturing links to pollution and waste through processes and products, not directly to NBSAP commitments |
| C-061 | s2 | s2_sowhat | LBG’s book | Definition | DEF-EXPOSURE |  | Group total exposure, £645,058m |
| C-070 | s3 | s3_sowhat | the next step is to set portfolio targets, scale resilience finance and strengthen exposure data | EY view | EV-016 | C-076, C-083, C-088, C-045 | Gaps: absence checks C-076 (resilience lending volume), C-083 (water lending target), C-088 (nature metric for agriculture) and C-045 (flood exposure of the book) |
| C-072 | s3 | s3_col1_today | Underwrote £238.5m for Cleve Hill with NatWest | Quoted fact | EV-025 |  |  |
| C-073 | s3 | s3_col1_today | coastal flood defences | Quoted fact | EV-066 |  |  |
| C-074 | s3 | s3_col1_today | Flood doors and barriers eligible for finance | Quoted fact | EV-058 |  |  |
| C-076 | s3 | s3_col1_gap | Volume of resilience lending (not yet visible) | Absence check | DEF-NOTVISIBLE | EV-017, EV-042 | SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); sustainable finance totals are not split by resilience |
| C-077 | s3 | s3_col1_opp | Scale resilience and remediation finance for homes and property, including flood-resilience retrofits and subsidence repair | EY view | EV-058 | EV-040, DEF-NOTVISIBLE | Eligibility already exists (EV-058) and the book is large (EV-040). Subsidence: SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); no mention found |
| C-078 | s3 | s3_col2_today | a £3bn aqueduct | Quoted fact | EV-064 |  |  |
| C-079 | s3 | s3_col2_today | around 2.5 million people | Quoted fact | EV-065 |  | "approximately 2.5 million people" |
| C-080 | s3 | s3_col2_today | Wastewater treatment eligible for finance | Quoted fact | EV-054 |  |  |
| C-081 | s3 | s3_col2_today | reservoirs eligible for finance | Quoted fact | EV-053 |  |  |
| C-082 | s3 | s3_col2_today | wetlands eligible for finance | Quoted fact | EV-074 |  |  |
| C-083 | s3 | s3_col2_gap | Water-specific lending target or volume (not yet visible) | Absence check | DEF-NOTVISIBLE | EV-043 | SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); only an own-operations water neutrality target (EV-043) found |
| C-084 | s3 | s3_col2_opp | Scale finance for drought resilience, storm-overflow reduction and water efficiency, including solutions for water-dependent growth sectors such as data centres | EY view | EV-019 | NB-M01, NB-M09, EV-063 | Drought (EV-019), storm overflow commitments (NB-M01, NB-M09), data centres eligible (EV-063) |
| C-085 | s3 | s3_col3_today | Regenerative farming eligible for finance | Quoted fact | EV-047 |  |  |
| C-086 | s3 | s3_col3_today | grant-supported projects are eligible | Quoted fact | EV-048 |  | "eligible for environmentally based subsidies or grants" (shortened; no quotation marks on the slide) |
| C-087 | s3 | s3_col3_today | 238,494 hectares | Quoted fact | EV-002 |  |  |
| C-088 | s3 | s3_col3_gap | A nature metric for the agriculture book (not yet visible) | Absence check | DEF-NOTVISIBLE | EV-041 | SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); agriculture target is emissions-based (EV-041) |
| C-089 | s3 | s3_col3_gap | The current target covers emissions only (−23% by 2030) | Quoted fact | EV-041 |  |  |
| C-090 | s3 | s3_col3_opp | Link Soil Association Exchange farm scorecards to LBG’s existing sustainability-linked loans, and report the lending volumes | EY view | EV-121 | EV-001, EV-002, EV-047 | Reworded after the 19 November review: the framework already offers sustainability-linked loans with borrower KPIs (EV-121) and soil-health eligibility (EV-047), so the opportunity is to connect the existing partnership scorecards (EV-001, EV-002) to that product and disclose volumes |
| C-092 | s3 | s3_sowhat | LBG has enabling frameworks, partnerships and flagship transactions in place | EY view | EV-016 | C-076, C-083, C-088 | Follows from the gaps row (absence checks) |
| C-100 | s1 | auto:s1_left_sub | Value of UK NBSAP commitments by theme (indicative) | Definition | DEF-COMMIT | CALC-02, ABB-NBSAP | Chart A subtitle; values in C-101 |
| C-101 | s1 | auto:chart A | Theme values and shares (£23.0bn 38%, £18.3bn 31%, £10.8bn 18%, £7.2bn 12%, £0.7bn 1%) | Calculated | CALC-V-DM | CALC-V-W, CALC-V-P, CALC-V-AFF, CALC-V-UD, CALC-S-DM, CALC-S-W, CALC-S-P, CALC-S-AFF, CALC-S-UD | Theme_View columns M and N |
| C-102 | s1 | auto:s1_callout_stat | 69% | Calculated | CALC-TOP2-S |  |  |
| C-103 | s2 | auto:r1-r5 | Theme scores 4.6, 4.5, 4.2, 3.8, 3.2 and sub-theme drill-downs | Calculated | CALC-TH-DM | CALC-TH-W, CALC-TH-AFF, CALC-TH-UD, CALC-TH-P, SC-DM01, SC-W02 | Theme_View; Scoring |
| C-104 | s2 | auto:s2_footnote | How we score (weights) | Definition | DEF-SCORE | DEF-TIER, DEF-EYVIEW | Rubric tab |
| C-105 | s2 | auto:theme names | Theme short names | Definition | DEF-THEME | DEF-TH-W, DEF-TH-AFF, DEF-TH-UD, DEF-TH-DM, DEF-TH-P |  |
| C-106 | s3 | auto:h1-h3 | Flood, coastal and adaptation 4.6; Water 4.5; Agriculture and forestry 4.2 | Calculated | CALC-TH-DM | CALC-TH-W, CALC-TH-AFF | Top three themes by score |
| C-107 | s1 | auto:notation | £bn, £m (bn and m are listed in the abbreviation key) | Definition | DEF-NOTATION |  |  |
| C-109 | s1 | auto:footnotes | EY view; NBSAP (in footnotes and auto text) | Definition | DEF-EYVIEW | ABB-EY, ABB-NBSAP | Structural text (footnotes) uses these terms |
| C-108 | s2 | auto:nature priority | yellow border = nature priority sector (exposure flag) | Definition | DEF-NATPRIOR |  |  |
| C-110 | s1 | auto:s1_ctx_what | NBSAP (National Biodiversity Strategy and Action Plan) | Abbreviation | ABB-NBSAP |  |  |
| C-111 | s1 | auto:s1_ctx_what | For the UK this is the Environmental Improvement Plan 2025 | Quoted fact | SRC-EY |  | EY workbook, Countries tab (GBR, NBSAP document) |
| C-112 | s1 | auto:s1_ctx_measured | 34 UK commitments with a stated amount | Calculated | CALC-01 | DEF-COMMIT |  |
| C-113 | s1 | auto:s1_ctx_measured | about £60bn, indicative | Calculated | CALC-02 |  | £60.0bn, shown as about £60bn |
| C-114 | s1 | auto:s1_ctx_measured | EY’s NBSAP sizing tool … to 5 themes | Definition | DEF-THEME | ABB-EY, ABB-NBSAP |  |
| C-117 | s1 | s1_ctx_why | UK nature finance gap at £56bn | Quoted fact | EV-008 |  | "the finance gap to meet the UK’s nature related outcomes in the UK is £56 billion" |
| C-119 | s1 | s1_sowhat | Financing water and flood resilience is how banks share in that growth | EY view | EV-080 | EV-095, EV-032, CALC-TOP2-S | Growth needs water (EV-080, EV-095); water and flood take 69% of UK NBSAP money (CALC-TOP2-S); LBG cites a $100–130bn adaptation financing opportunity for banks (EV-032) |
| C-120 | s1 | s1_ctx_why | Over half (51%) of UK economic output depends highly on nature | Quoted fact | EV-103 | DEF-OUTPUT | ENCORE "GVA % High Dep" for GB = 51.38, shown as 51% |
| C-146 | s2 | auto:r1-r5 align | ✓ In LBG’s stated priorities (flood, coastal and adaptation; water) | Definition | DEF-ALIGN | EV-115 | Built to Last names flood preparedness and drought resilience among LBG’s three critical priorities; other themes not named as LBG priorities in the documents reviewed |
| C-147 | s3 | s3_title | 9.3% | Calculated | CALC-59 | EV-034, EV-067 | Nature priority sectors £59,991m ÷ Group £645,058m |
| C-150 | s3 | s3_exp_stat | 9.3% | Calculated | CALC-59 | EV-034, EV-067 | Nature priority sectors £59,991m ÷ Group £645,058m |
| C-151 | s3 | s3_exp_sub | £645.1bn | Calculated | CALC-51 | EV-067 | £645,058m ÷ 1,000, 1 d.p. |
| C-152 | s3 | s3_exp_sub | 31 December 2025 | Quoted fact | EV-067 |  | Tab 18 column header "31st December 2025" |
| C-153 | s3 | s3_exp_sub | Share of total exposure by sector | Definition | DEF-EXPOSURE |  | Total exposure = lending + off-balance sheet |
| C-154 | s3 | s3_exp_text | £60.0bn | Quoted fact | EV-034 |  | £59,991m ÷ 1,000, 1 d.p. |
| C-155 | s3 | s3_exp_text | sectors LBG classes as nature priorities | Definition | DEF-NATPRIOR | EV-034 | LBG’s own classification (data sheet tab 18, footnote 12) |
| C-156 | s3 | s3_exp_note | LBG classes mortgages as 0% nature priority, although they carry physical risks such as flooding | Quoted fact | EV-040 | EV-069, EV-120 | Tab 18: mortgages 0.0% nature priority (EV-040); footnote 11 (EV-120). "Physical risks such as flooding" = EY view (C-045, C-046) |
| C-158 | s3 | auto:chart D | Book by sector: each sector total as % of the £645.1bn Group total; yellow = disclosed nature-priority % | Calculated | CALC-51 | EV-040, EV-118, EV-119, EV-099, EV-068, EV-117, EV-045, EV-038, EV-100, EV-075, EV-039, EV-116 | Deck_Chart A30:C41 = Client_Exposure sector totals ÷ Group total (QA 27: sector totals add up to the Group total); nature-priority % copied from tab 18 (Client_Exposure K) |
| C-159 | s1 | auto:abbreviation keys | Abbreviation key at the foot of pages 2–4 | Definition | ABB-NBSAP | ABB-EY, ABB-ENCORE, ABB-LBG, ABB-UK, ABB-PRA, ABB-SS525, ABB-AI, ABB-BN, ABB-M | Built by Excel from Glossary column G (key text) for every abbreviation found on the page (Glossary I:K) |
| C-160 | s2 | s2_title | 81% | Calculated | CALC-TOP3-S | CALC-S-DM, CALC-S-W, CALC-S-AFF | 38.3% + 30.5% + 12.0% = 80.7%, shown as 81% (top three themes by score) |
| C-161 | s2 | s2_title | Three priority themes | Definition | DEF-SCORE | CALC-TH-DM, CALC-TH-W, CALC-TH-AFF | The three highest-scoring themes (4.6, 4.5, 4.2) |
| C-162 | s2 | s2_link_UD | direct link through development and land-use pressures | EY view | EV-039 | EV-076, EV-005 | Construction and housebuilding drive land-use change; biodiversity net gain applies to new developments (EV-005) |
| C-163 | s2 | s2_other1_text | point to net gain finance for housebuilders | EY view | EV-005 | EV-039, EV-076, EV-024 | Mandatory 10% net gain for new developments (EV-005); £2.3bn housebuilder lending (EV-076); LBG already finances a project with a net gain commitment (EV-024) |
| C-164 | s2 | s2_other2_text | 18% | Calculated | CALC-S-P |  | Pollution and waste share of UK commitments |
| C-165 | s2 | s2_other2_text | £10.8bn | Calculated | CALC-V-P |  |  |
| C-166 | s2 | s2_other2_text | a weak link in LBG’s book: scope to grow waste and circular-economy finance under its existing framework | EY view | EV-060 | EV-044, SC-P02 | Theme score 3.2 (lowest; materiality to LBG 2); framework already has circular-economy criteria (EV-060); LBG flags waste management as nature-relevant (EV-044) |
| C-167 | s2 | auto:r1-r5 UK tick | ✓ = theme takes 10% or more of identified UK commitments (flood 38%, water 31%, pollution 18%, agriculture 12%) | Definition | DEF-UKPRIO | CALC-S-DM, CALC-S-W, CALC-S-P, CALC-S-AFF, CALC-S-UD | Theme_View X5:X9; threshold Theme_View X2 |
| C-168 | s3 | s3_col3_today | sustainability-linked loans already use borrower indicators | Quoted fact | EV-121 |  | Paraphrase, no quotation marks (financing framework page 24) |

## 5. Self-check before replying

1. Every key in SRC-METH section 10 is present and within its limit.
2. Every non-structural value has Claim_IDs, and every Claim_ID appears in my Claims_Register table.
3. Every Calculated claim shows exactly the CALC value (same rounding); every Quoted fact matches its EV quote, with any unit change written in the derivation.
4. Quotation marks only around exact source words.
5. Words are written in full wherever space allows; every abbreviation kept has an ABB- entry with a sourced expansion and key text; no e.g., c., incl., ha, vs or shortened months.
6. Every EY view has a rationale and cites evidence; every absence check lists the sources searched.
7. Action titles state findings in 68 characters or fewer; neutral tone; no names, fees or EY confidential data.
8. No text was copied from the LBG example except structure and style.
