# Slide Writing Guide and Worked Example (v4)

Knowledge file for the Copilot agent. It shows how to write every Slide_Content value for pages 2–4, and the Claims_Register rows behind them. It is used with SRC-METH (02_Methodology_and_Specifications). Page 5 is written by the EY team.

## 1. Rules

1. **Copy the style, never the facts.** The worked example is Lloyds Banking Group (UK). Never reuse its names, figures or wording for another client.
2. **Trace everything.** Every value except structural keys lists Claim_IDs, and every Claim_ID has a Claims_Register row whose primary trace resolves (EV-, NB-, CALC-, SC-, DEF-, ABB-, SRC-). No trace, no text.
3. **Numbers come from the workbook.** Percentages, totals, theme values and scores must equal a CALC- or SC- value. Exposure figures are quoted (EV-) and converted only for units (£365,599m → £365.6bn), with the conversion written in the claim derivation.
4. **Action titles state the finding**, in 68 characters or fewer. Write "LBG meets that money mainly through flood risk in its mortgage book", not "Exposure analysis".
5. **Every abbreviation needs an ABB- glossary entry** whose expansion comes from a source. If no source spells it out (e.g. "ELMs" in the LBG framework), reword instead of abbreviating.
6. **Facts, gaps and views are separate.** Page 4 "today" is facts only. "Gap" is an absence check that lists the sources searched (never "LBG fails to…"). "Opportunity" is an EY view with a rationale.
7. **Say "not yet visible"**, not "missing" or "lacking". The footnote says which documents were reviewed and which were not (e.g. the annual report).
8. **Write money and units consistently:** £12.1bn, £120m, 9.3%, c.£41bn (c. = rounded), 238,494 ha. Use the country's currency symbol.
9. **One message per field.** The tile text says what the figure is and its scope; the "So what" says why it matters to the client.
10. **Use UK English and a neutral tone.** Never name individuals, never use EY confidential data, never mention fees.
11. **Never draft page 5.** It is written by the EY team.
12. **Return only the two tables** (Slide_Content; Claims_Register) in key order, with no blank keys.

## 2. Field guide (limit = maximum characters)

| Key | What to write | Limit | Good (LBG example) | Weak | Why weak |
|---|---|---|---|---|---|
| s1_title | Action title: the finding about the national context, ≤68 characters | 68 | Water and flood take most UK nature money, and growth will add more | UK nature context | A topic label, not a finding. |
| s1_callout_text | Words after the automatic % (e.g. "goes to water and flood together (c.£41bn)"); cite CALC-TOP2-V | 55 | goes to water and flood together (c.£41bn) | is a lot of money | No subject; no traced value. |
| s1_tile1_stat | Tile 1 figure, e.g. £725bn | 8 | £725bn | Huge | Not a figure. |
| s1_tile1_text | What the figure is, with scope | 85 | UK 10-year infrastructure pipeline, incl. £7.9bn for flood management (2026–36) | infrastructure spending is going up | No scope, no source figure. |
| s1_tile4_text | Text; label EY view in the claim | 85 | Data centres, new homes and infrastructure all need water, flood protection and land | Growth is important | Says nothing; an EY view needs a rationale and evidence. |
| s1_rules | Regulations with dates, ≤1 line | 120 | 10% biodiversity net gain (BNG) mandatory since Feb 2024; PRA SS5/25 raises physical-risk expectations for banks. | Regulation is getting stricter | No rule named, no date. |
| s1_sowhat | One sentence: what the context means for banks / the client sector | 150 | Water and flood resilience are becoming the UK’s investment priority. For banks this is a growth market to finance, not only a risk to manage. | Banks should look at nature | Not specific to the evidence. |
| s2_title | Action title: how the context meets the client, ≤68 characters | 68 | LBG meets that money mainly through flood risk in its mortgage book | Client exposure | A label, not the finding. |
| s2_link_W | Main client link for the Water theme: sector, £ and % of book | 105 | Energy and water supply £12.1bn (1.9%); £120m to the HARP aqueduct. | Utilities ~£12bn | Approximate; no % of book; informal sector name. |
| s2_link_DM | Main client link for Flood, coastal and adaptation | 105 | Mortgages £365.6bn (57%) and property £26.3bn (4%). Flood exposure of the book not disclosed. | Lots of mortgages exposed to floods (est. £100bn) | Estimated; never estimate. Quote and calculate. |
| s2_sowhat | One sentence: the gap between where the money goes and the client book | 190 | 69% of UK nature money targets water and flood, but energy and water supply is only 1.9% of LBG’s book. LBG’s largest link is indirect: flood risk across £365.6bn of mortgages. | The table shows the themes and exposures. | Describes the slide instead of the insight. |
| s3_col1_today | Theme ranked 1: what the client does today (facts only) | 185 | Published Built to Last (2026) on climate resilience. With NatWest, underwrote £238.5m for Cleve Hill, which has coastal flood defences. Flood doors and barriers eligible for finance. | LBG is doing lots on flooding | No traced facts. |
| s3_col1_gap | Theme ranked 1: not yet visible in the documents reviewed | 150 | Flood exposure of the £365.6bn mortgage book (LBG’s mortgage climate flag reflects home energy use). Volume of resilience lending. | LBG has failed to disclose flood risk | Critical tone; absence must be "not yet visible" with sources searched. |
| s3_col1_opp | Theme ranked 1: opportunity (EY view) | 125 | Resilience and remediation lending for homes and property, e.g. flood retrofit and subsidence repair. | Big opportunity | Not specific; no rationale. |
| s3_sowhat | One sentence: the next step for the client | 170 | LBG already has the framework, partnerships and flagship deals. The next step is scale: targets, lending volumes and portfolio data for water and flood. | EY can help with many things | Not the client's next step; page 5 covers EY. |

Structural keys (no Claim_IDs needed): client_name, cover_date, cover_title, s1_footnote, s1_heading_r, s1_rules_lead, s1_section, s1_talk, s2_section, s2_talk, s3_footnote, s3_section, s3_talk.

## 3. Worked example: Lloyds Banking Group (UK), Slide_Content

| Key | Value | Claim_IDs | Limit | Length |
|---|---|---|---|---|
| cover_title | Nature opportunities for Lloyds Banking Group |  | 60 | 45 |
| cover_date | 10th December 2026 |  | 25 | 18 |
| client_name | Lloyds Banking Group |  | 40 | 20 |
| client_short | LBG | C-001 | 12 | 3 |
| s1_title | Water and flood take most UK nature money, and growth will add more | C-002, C-010, C-011 | 68 | 67 |
| s1_section | The UK context |  | 30 | 14 |
| s1_callout_text | goes to water and flood together (c.£41bn) | C-012, C-013 | 55 | 42 |
| s1_heading_r | Where the UK is heading |  | 35 | 23 |
| s1_sub_r | Growth, drought and regulation all point the same way | C-020 | 70 | 53 |
| s1_tile1_stat | £725bn | C-021 | 8 | 6 |
| s1_tile1_text | UK 10-year infrastructure pipeline, incl. £7.9bn for flood management (2026–36) | C-021, C-022 | 85 | 79 |
| s1_tile2_stat | 25m+ | C-023 | 8 | 4 |
| s1_tile2_text | people under water-use restrictions in the 2026 drought; reservoirs fell to 69% | C-023, C-024 | 85 | 79 |
| s1_tile3_stat | £11bn | C-025 | 8 | 5 |
| s1_tile3_text | a year gap in UK adaptation investment (Climate Change Committee) | C-025, C-026 | 85 | 65 |
| s1_tile4_stat | Growth | C-027 | 8 | 6 |
| s1_tile4_text | Data centres, new homes and infrastructure all need water, flood protection and land | C-027 | 85 | 84 |
| s1_rules_lead | Rules are tightening: |  | 24 | 21 |
| s1_rules | 10% biodiversity net gain (BNG) mandatory since Feb 2024; PRA SS5/25 raises physical-risk expectations for banks. | C-028, C-029, C-030, C-031, C-032 | 120 | 113 |
| s1_sowhat | Water and flood resilience are becoming the UK’s investment priority. For banks this is a growth market to finance, not only a risk to manage. | C-033, C-034 | 150 | 142 |
| s1_footnote | Sources: EY NBSAP Nature Sizing workbook (UK commitments; indicative, timeframes vary; EY classification review); LBG Built to Last (2026), PDF pp.3, 7, 10, 11, citing the UK Government and the Climate Change Committee; LBG Working with nature (2024) pp.3, 7; LBG Sustainable Financing Framework 2025, p.11. Growth tile: EY view. |  | 330 | 329 |
| s1_talk | Start with the money, not the policy. Of c.£60bn of UK NBSAP commitments in the EY sizing tool, 69% goes to water and flood. Then the direction of travel: the £725bn infrastructure pipeline, the 2026 drought, and growth sectors such as data centres and housing that all need water and land. Close: this is a growth market for banks, not only a risk topic. |  | 700 | 355 |
| s2_title | LBG meets that money mainly through flood risk in its mortgage book | C-001, C-040 | 68 | 67 |
| s2_section | What it means for LBG |  | 30 | 21 |
| s2_link_W | Energy and water supply £12.1bn (1.9%); £120m to the HARP aqueduct. | C-047, C-048, C-049, C-050 | 105 | 67 |
| s2_link_AFF | Agriculture, forestry and fishing £6.8bn (1.1%); 685 farms in LBG’s soil partnership. | C-051, C-052, C-053, C-054 | 105 | 85 |
| s2_link_UD | Construction £5.9bn (0.9%), incl. £2.3bn housebuilders. | C-055, C-056, C-057 | 105 | 55 |
| s2_link_DM | Mortgages £365.6bn (57%) and property £26.3bn (4%). Flood exposure of the book not disclosed. | C-041, C-042, C-043, C-044, C-045, C-046 | 105 | 93 |
| s2_link_P | Manufacturing £13.1bn (2.0%); indirect link. | C-058, C-059, C-060 | 105 | 44 |
| s2_sowhat | 69% of UK nature money targets water and flood, but energy and water supply is only 1.9% of LBG’s book. LBG’s largest link is indirect: flood risk across £365.6bn of mortgages. | C-010, C-048, C-061, C-041, C-046 | 190 | 176 |
| s2_talk | Same themes, now set against LBG. Read across each row: how much UK money goes there, and where LBG’s book touches it. Water and flood lead on priority, but LBG’s direct water lending is small (1.9% of the book). The big link is the mortgage book, through flood risk. If asked why flood scores 4.6 and water 4.5: both are water-related and together they are 69% of UK commitments. |  | 700 | 380 |
| s3_title | LBG has the foundations; the gaps are scale, targets and disclosure | C-070 | 68 | 67 |
| s3_section | LBG today: strengths and gaps |  | 40 | 29 |
| s3_col1_today | Published Built to Last (2026) on climate resilience. With NatWest, underwrote £238.5m for Cleve Hill, which has coastal flood defences. Flood doors and barriers eligible for finance. | C-071, C-072, C-073, C-074 | 185 | 183 |
| s3_col1_gap | Flood exposure of the £365.6bn mortgage book (LBG’s mortgage climate flag reflects home energy use). Volume of resilience lending. | C-045, C-041, C-075, C-076 | 150 | 130 |
| s3_col1_opp | Resilience and remediation lending for homes and property, e.g. flood retrofit and subsidence repair. | C-077 | 125 | 101 |
| s3_col2_today | £120m of debt to HARP, a £3bn aqueduct supplying c.2.5m people. Wastewater treatment, reservoirs and wetlands eligible for finance. | C-049, C-050, C-078, C-079, C-080, C-081, C-082 | 185 | 131 |
| s3_col2_gap | Water-specific lending target or volume. Split of the £12.1bn energy and water supply book. | C-083, C-047 | 150 | 91 |
| s3_col2_opp | Finance water build-out: drought resilience, storm overflows and water efficiency for growth sectors such as data centres. | C-084 | 125 | 122 |
| s3_col3_today | Ten-year Soil Association Exchange partnership: 685 farms, 238,494 ha. Regenerative farming and subsidy- or grant-backed projects eligible for finance. | C-054, C-053, C-085, C-086, C-087 | 185 | 151 |
| s3_col3_gap | A nature metric for the £6.8bn agriculture book. The current target covers emissions only (−23% by 2030). | C-088, C-051, C-089 | 150 | 105 |
| s3_col3_opp | Nature-linked farm lending, with soil and water outcomes as loan KPIs. | C-090, C-091 | 125 | 70 |
| s3_sowhat | LBG already has the framework, partnerships and flagship deals. The next step is scale: targets, lending volumes and portfolio data for water and flood. | C-092 | 170 | 152 |
| s3_footnote | Sources: LBG Working with nature (2024) p.4; Built to Last (2026) PDF pp.14–17; Sustainability Data Sheet 2025, tabs 18 and 20; Sustainable Financing Framework 2025 pp.11–15. "Not yet visible" = not found in these four documents; to be checked against the Annual and Sustainability Reports 2025. Scores from page 3. |  | 330 | 315 |
| s3_talk | Credit LBG first: HARP, Cleve Hill, the Soil Association partnership, and a financing framework that already covers water, flood resilience and regenerative farming. Then the gaps, framed as not yet visible: portfolio flood exposure, lending volumes and nature targets. Then the opportunity row. Keep the conversation on opportunity, not risk. |  | 700 | 343 |

## 4. Worked example: Claims_Register (all rows)

| Claim_ID | Slide | Key | Claim | Type | Primary trace | Other traces | Derivation / how checked |
|---|---|---|---|---|---|---|---|
| C-001 | s2 | client_short | LBG | Abbreviation | ABB-LBG |  | Expansion in Glossary |
| C-002 | s1 | s1_title | UK | Abbreviation | ABB-UK |  |  |
| C-010 | s1 | s1_title | 69% (water and flood take most UK nature money) | Calculated | CALC-TOP2-S | CALC-S-DM, CALC-S-W | Top two themes by value = Flood, coastal and adaptation (38%) + Water (31%); after classification review (NB-M01, NB-M09, NB-M120, NB-M121) |
| C-011 | s1 | s1_title | growth will add more | EY view | EV-061 | EV-063, EV-072 | Infrastructure pipeline (EV-061), data centres (EV-063) and housing growth (EV-072) all depend on water, flood protection and land |
| C-012 | s1 | s1_callout_text | £41bn | Calculated | CALC-TOP2-V | CALC-V-DM, CALC-V-W | £23.0bn + £18.3bn = £41.3bn, shown as c.£41bn |
| C-013 | s1 | s1_callout_text | water and flood | Definition | DEF-TH-DM | DEF-TH-W | "flood" = theme Flood, coastal and adaptation (Disaster management and adaptation) |
| C-020 | s1 | s1_sub_r | Growth, drought and regulation all point the same way | EY view | EV-061 | EV-019, EV-070 | Summary of the four tiles and the rules line |
| C-021 | s1 | s1_tile1_stat | £725bn UK 10-year infrastructure pipeline | Quoted fact | EV-061 |  | UK Infrastructure: A 10-Year Strategy, as cited by LBG |
| C-022 | s1 | s1_tile1_text | £7.9bn for flood management (2026–36) | Quoted fact | EV-020 |  |  |
| C-023 | s1 | s1_tile2_stat | 25m+ people under water-use restrictions in the 2026 drought | Quoted fact | EV-062 | EV-019 | "more than 25 million people" (UK Government 2026, as cited by LBG) |
| C-024 | s1 | s1_tile2_text | reservoirs fell to 69% | Quoted fact | EV-019 |  |  |
| C-025 | s1 | s1_tile3_stat | £11bn a year gap in UK adaptation investment | Quoted fact | EV-018 |  |  |
| C-026 | s1 | s1_tile3_text | Climate Change Committee | Quoted fact | EV-071 |  | Source of the £11bn figure |
| C-027 | s1 | s1_tile4_text | Data centres, new homes and infrastructure all need water, flood protection and land | EY view | EV-063 | EV-072, EV-061, DEF-W01 | Data centres in LBG’s financing framework (EV-063); housing growth needs NbS for sewer and flood pressures (EV-072); infrastructure pipeline (EV-061) |
| C-028 | s1 | s1_rules | 10% biodiversity net gain mandatory since Feb 2024 | Quoted fact | EV-005 |  |  |
| C-029 | s1 | s1_rules | BNG | Abbreviation | ABB-BNG |  |  |
| C-030 | s1 | s1_rules | PRA SS5/25 raises physical-risk expectations for banks | Quoted fact | EV-070 | EV-073 |  |
| C-031 | s1 | s1_rules | PRA | Abbreviation | ABB-PRA |  |  |
| C-032 | s1 | s1_rules | SS5/25 | Abbreviation | ABB-SS525 |  |  |
| C-033 | s1 | s1_sowhat | Water and flood resilience are becoming the UK’s investment priority | EY view | CALC-TOP2-S | EV-020, EV-061 | 69% of UK NBSAP commitments (CALC-TOP2-S) plus £7.9bn flood allocation in the infrastructure pipeline (EV-020) |
| C-034 | s1 | s1_sowhat | For banks this is a growth market to finance, not only a risk to manage | EY view | EV-032 | EV-013 | LBG cites a $100–130bn adaptation financing opportunity for banks (EV-032) and a role for private finance (EV-013) |
| C-040 | s2 | s2_title | LBG meets that money mainly through flood risk in its mortgage book | EY view | EV-040 | CALC-52, C-045 | Mortgages are 57% of Group exposure; flood is the largest-value theme; flood exposure of the book is not disclosed |
| C-041 | s2 | s2_link_DM | £365.6bn mortgages | Quoted fact | EV-040 |  | £365,599m ÷ 1,000, 1 d.p. |
| C-042 | s2 | s2_link_DM | 57% | Calculated | CALC-52 |  |  |
| C-043 | s2 | s2_link_DM | property £26.3bn | Quoted fact | EV-068 |  | £26,270m ÷ 1,000, 1 d.p. |
| C-044 | s2 | s2_link_DM | 4% | Calculated | CALC-53 |  |  |
| C-045 | s2 | s2_link_DM | Flood exposure of the book not disclosed | Absence check | SRC-04 | EV-069 | SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); tab 18 reports climate (energy) and nature-priority flags only |
| C-046 | s2 | s2_link_DM | Mortgages and property linked to Flood, coastal and adaptation | EY view | EV-040 | EV-058, EV-066 | Physical flood risk to homes and property; LBG already finances flood resilience products (EV-058) |
| C-047 | s2 | s2_link_W | Energy and water supply £12.1bn | Quoted fact | EV-038 |  | £12,084m ÷ 1,000, 1 d.p. |
| C-048 | s2 | s2_link_W | 1.9% | Calculated | CALC-54 |  | Not the 1.7% nature-priority share shown in tab 18; this is total sector exposure ÷ Group total |
| C-049 | s2 | s2_link_W | £120m to the HARP aqueduct | Quoted fact | EV-023 | EV-064 |  |
| C-050 | s2 | s2_link_W | HARP | Abbreviation | ABB-HARP |  |  |
| C-051 | s2 | s2_link_AFF | Agriculture, forestry and fishing £6.8bn | Quoted fact | EV-075 |  | £6,776m ÷ 1,000, 1 d.p. |
| C-052 | s2 | s2_link_AFF | 1.1% | Calculated | CALC-55 |  |  |
| C-053 | s2 | s2_link_AFF | 685 farms | Quoted fact | EV-002 |  |  |
| C-054 | s2 | s2_link_AFF | LBG’s soil partnership (Soil Association Exchange, ten-year) | Quoted fact | EV-001 |  |  |
| C-055 | s2 | s2_link_UD | Construction £5.9bn | Quoted fact | EV-039 |  | £5,850m ÷ 1,000, 1 d.p. |
| C-056 | s2 | s2_link_UD | 0.9% | Calculated | CALC-57 |  |  |
| C-057 | s2 | s2_link_UD | £2.3bn housebuilders | Quoted fact | EV-076 |  | £2,262m ÷ 1,000, 1 d.p. |
| C-058 | s2 | s2_link_P | Manufacturing £13.1bn | Quoted fact | EV-045 |  | £13,095m ÷ 1,000, 1 d.p. |
| C-059 | s2 | s2_link_P | 2.0% | Calculated | CALC-56 |  |  |
| C-060 | s2 | s2_link_P | indirect link | EY view | EV-045 | EV-059, EV-060 | Manufacturing links to pollution and waste through processes and products, not directly to NBSAP commitments |
| C-061 | s2 | s2_sowhat | LBG’s book | Definition | DEF-EXPOSURE |  | Group total exposure, £645,058m |
| C-070 | s3 | s3_title | LBG has the foundations; the gaps are scale, targets and disclosure | EY view | EV-016 | EV-046, C-076, C-083, C-088 | Foundations: £100bn pledge (EV-016), framework (EV-046), deals on this page; gaps: absence checks C-045, C-076, C-083, C-088 |
| C-071 | s3 | s3_col1_today | Published Built to Last (2026) on climate resilience | Quoted fact | SRC-02 |  | The source document itself |
| C-072 | s3 | s3_col1_today | With NatWest, underwrote £238.5m for Cleve Hill | Quoted fact | EV-025 |  |  |
| C-073 | s3 | s3_col1_today | coastal flood defences | Quoted fact | EV-066 |  |  |
| C-074 | s3 | s3_col1_today | Flood doors and barriers eligible for finance | Quoted fact | EV-058 |  |  |
| C-075 | s3 | s3_col1_gap | LBG’s mortgage climate flag reflects home energy use | Quoted fact | EV-069 |  |  |
| C-076 | s3 | s3_col1_gap | Volume of resilience lending (not yet visible) | Absence check | DEF-NOTVISIBLE | EV-017, EV-042 | SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); sustainable finance totals are not split by resilience |
| C-077 | s3 | s3_col1_opp | Resilience and remediation lending for homes and property, e.g. flood retrofit and subsidence repair | EY view | EV-058 | EV-040, DEF-NOTVISIBLE | Eligibility already exists (EV-058) and the book is large (EV-040). Subsidence: SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); no mention found |
| C-078 | s3 | s3_col2_today | a £3bn aqueduct | Quoted fact | EV-064 |  |  |
| C-079 | s3 | s3_col2_today | supplying c.2.5m people | Quoted fact | EV-065 |  | "approximately 2.5 million people" |
| C-080 | s3 | s3_col2_today | Wastewater treatment eligible for finance | Quoted fact | EV-054 |  |  |
| C-081 | s3 | s3_col2_today | reservoirs eligible for finance | Quoted fact | EV-053 |  |  |
| C-082 | s3 | s3_col2_today | wetlands eligible for finance | Quoted fact | EV-074 |  |  |
| C-083 | s3 | s3_col2_gap | Water-specific lending target or volume (not yet visible) | Absence check | DEF-NOTVISIBLE | EV-043 | SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); only an own-operations water neutrality target (EV-043) found |
| C-084 | s3 | s3_col2_opp | Finance water build-out: drought resilience, storm overflows and water efficiency for growth sectors such as data centres | EY view | EV-019 | NB-M01, NB-M09, EV-063 | Drought (EV-019), storm overflow commitments (NB-M01, NB-M09), data centres eligible (EV-063) |
| C-085 | s3 | s3_col3_today | Regenerative farming eligible for finance | Quoted fact | EV-047 |  |  |
| C-086 | s3 | s3_col3_today | subsidy- or grant-backed projects eligible for finance | Quoted fact | EV-048 |  | "eligible for environmentally based subsidies or grants" |
| C-087 | s3 | s3_col3_today | 238,494 ha | Quoted fact | EV-002 | DEF-NOTATION |  |
| C-088 | s3 | s3_col3_gap | A nature metric for the agriculture book (not yet visible) | Absence check | DEF-NOTVISIBLE | EV-041 | SRC-01, SRC-02, SRC-04, SRC-05 searched (full text / all tabs); agriculture target is emissions-based (EV-041) |
| C-089 | s3 | s3_col3_gap | The current target covers emissions only (−23% by 2030) | Quoted fact | EV-041 |  |  |
| C-090 | s3 | s3_col3_opp | Nature-linked farm lending, with soil and water outcomes as loan KPIs | EY view | EV-011 | EV-001, EV-002 | LBG calls for green finance to farmers (EV-011) and already measures soil and water on 685 farms (EV-001, EV-002) |
| C-091 | s3 | s3_col3_opp | KPIs | Abbreviation | ABB-KPI |  |  |
| C-092 | s3 | s3_sowhat | The next step is scale: targets, lending volumes and portfolio data for water and flood | EY view | EV-016 | C-076, C-083, C-088 | Follows from the gaps row (absence checks) |
| C-100 | s1 | auto:s1_left_sub | 34 UK NBSAP commitments, c.£60bn in total | Calculated | CALC-02 | CALC-01, ABB-NBSAP | Count and sum of UK commitments (display currency GBP) |
| C-101 | s1 | auto:chart A | Theme values and shares (£23.0bn 38%, £18.3bn 31%, £10.8bn 18%, £7.2bn 12%, £0.7bn 1%) | Calculated | CALC-V-DM | CALC-V-W, CALC-V-P, CALC-V-AFF, CALC-V-UD, CALC-S-DM, CALC-S-W, CALC-S-P, CALC-S-AFF, CALC-S-UD | Theme_View columns M and N |
| C-102 | s1 | auto:s1_callout_stat | 69% | Calculated | CALC-TOP2-S |  |  |
| C-103 | s2 | auto:r1-r5 | Theme scores 4.6, 4.5, 4.2, 3.8, 3.2 and sub-theme drill-downs | Calculated | CALC-TH-DM | CALC-TH-W, CALC-TH-AFF, CALC-TH-UD, CALC-TH-P, SC-DM01, SC-W02 | Theme_View; Scoring |
| C-104 | s2 | auto:s2_footnote | How we score (weights) | Definition | DEF-SCORE | DEF-TIER, DEF-EYVIEW | Rubric tab |
| C-105 | s2 | auto:theme names | Theme short names | Definition | DEF-THEME | DEF-TH-W, DEF-TH-AFF, DEF-TH-UD, DEF-TH-DM, DEF-TH-P |  |
| C-106 | s3 | auto:h1-h3 | Flood, coastal and adaptation 4.6; Water 4.5; Agriculture and forestry 4.2 | Calculated | CALC-TH-DM | CALC-TH-W, CALC-TH-AFF | Top three themes by score |
| C-107 | s1 | auto:notation | £bn, £m, ha, c. | Definition | DEF-NOTATION |  |  |
| C-109 | s1 | auto:footnotes | EY view; NBSAP (in footnotes and auto text) | Definition | DEF-EYVIEW | ABB-EY, ABB-NBSAP | Structural text (footnotes) uses these terms |
| C-108 | s2 | auto:nature priority | yellow border = nature priority sector (exposure flag) | Definition | DEF-NATPRIOR |  |  |

## 5. Self-check before replying

1. Every key in SRC-METH section 10 is present, in order, and within its limit.
2. Every non-structural value has Claim_IDs, and every Claim_ID appears in my Claims_Register table.
3. Every Calculated claim shows exactly the CALC value (same rounding); every Quoted fact matches its EV quote, with any unit change written in the derivation.
4. Every abbreviation used has an ABB- entry with a sourced expansion; no unsourced abbreviations.
5. Every EY view has a rationale and cites evidence; every absence check lists the sources searched.
6. Action titles state findings in 68 characters or fewer; tone is neutral; no names, fees or EY confidential data.
7. No text was copied from the LBG example except structure and style.
