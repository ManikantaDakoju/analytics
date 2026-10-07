# Setup guide (for you, not the agent)

## 1. Build the agent
Menu names vary by Copilot version and by what EY has enabled, so check the steps against your setup.

1. In Microsoft 365 Copilot, choose **Create agent** (or use Copilot Studio if that's what EY provides) and open the **Configure** tab.
2. **Name:** Nature Priorities Analyst
3. **Description:** Maps a client's public nature disclosures to the national NBSAP, scores priorities and drafts a 3-slide summary.
4. **Instructions:** paste the full contents of `01_Agent_Instructions.md`. The instructions field often has a character limit, commonly around 8,000 characters. This file is designed to fit under it.
5. **Knowledge:** upload these files:
   - `02_Methodology_and_Specifications` (save it as .docx or .pdf first, after filling in the placeholders)
   - Your Master Workbook, with the Taxonomy and NBSAP_Targets sheets completed
   - The UK NBSAP document
   - LBG's public reports
   - Optional: an EY CCaSS services list, if you're allowed to use it in this tool
6. **Web search:** turn it off for the proof of concept. Upload the sources yourself instead. This gives you control over what the agent reads and makes results repeatable.
7. **Starter prompts:**
   - "Start a new analysis for [client], [country], [sector]."
   - "Run the quality check again and show what failed."
   - "Draft the 3 slides from the confirmed scoring."

## 2. Getting the EY template to work
Copilot agents and Copilot chat usually can't apply a PowerPoint template (.potx) to a deck they create. That's most likely why your attempts haven't worked. The agent is therefore set up to output text mapped to placeholders, not a styled file. Then use one of these routes:

**Route A (try first): Copilot inside PowerPoint**
1. In PowerPoint, create a new presentation from the EY template. Use File > New and pick the EY template from your organisation's templates.
2. Open Copilot in PowerPoint and paste the agent's slide output.
3. Prompt: "Add 3 slides using the layouts in this presentation's slide master. Use this content exactly as written. Do not add, remove or rephrase any text."
4. Check each slide against the agent's output, especially the numbers.

**Route B (most reliable): manual placement**
1. Open the EY template and insert the 3 layouts named in the output, e.g. Title and content, or Title and table.
2. Paste each [Title], [Body], [Table] and [Footnote] block into its matching placeholder. This takes about 10 minutes and gives full template compliance.

**Route C (if EY has set it up):** Your organisation may have a branded template library that Copilot can use when creating presentations. Ask your local IT or the Copilot champions network.

## 3. Before your first run, fill in:
- [ ] Taxonomy sheet: themes, sub-themes, definitions and market opportunity scores from your tool
- [ ] NBSAP_Targets sheet: UK targets from the NBSAP
- [ ] Rubric (Section B): replace it if EY has an approved rubric; otherwise use the default
- [ ] Sheet and column names (Section A): match them to your Master Workbook
- [ ] EY services list for slide 3, if one can be used

## 4. Quick test
Run it once with 2 to 3 LBG documents. Check 5 random evidence rows against the source pages. If the quotes and pages are correct, run the full set.
