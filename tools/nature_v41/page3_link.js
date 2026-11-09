// Skeleton page 3 (v4.5): theme rows (score + money charts) with strategy tags, plus a growth-vs-book strip. Placeholders only.
const pptxgen = require('pptxgenjs');
const D = JSON.parse(require('fs').readFileSync(process.argv[2], 'utf8'));
const C = { FONT: 'EYInterstate Light', FB: 'EYInterstate', Y: 'FFE600', WH: 'F2F2F2', GR: 'BFBFC7', CARD: '2E2E38', LINE: '3A3A46', MED: 'A5A5B0', DARK: '1A1A24' };
const tx = (o) => Object.assign({ isTextBox: true, margin: 0, valign: 'top', fontFace: C.FONT, color: C.WH }, o);
const w = (t, o = {}) => ({ text: t, options: Object.assign({ color: C.WH, fontFace: C.FONT }, o) });
const pres = new pptxgen(); pres.layout = 'LAYOUT_WIDE'; const s = pres.addSlide();
const LX = 0.58, TOP = 1.25, COL = { score: 4.1, money: 6.05, mtxt: 7.1, link: 8.35 };
[['Theme', LX, 3.0], ['Priority (score out of 5)', COL.score, 1.9], ['{{country_label}} nature money ({{currency_symbol}}bn, share)', COL.money, 2.25], ['Main link to {{client_short}}’s book (% of total exposure)', COL.link, 3.69]]
  .forEach((h, i) => s.addText(h[0], tx({ x: h[1], y: TOP, w: h[2], h: 0.26, fontSize: 9.5, bold: true, fontFace: C.FB, color: C.Y, objectName: `col-head-${i + 1}` })));
s.addShape(pres.shapes.LINE, { x: LX, y: TOP + 0.3, w: 11.46, h: 0, line: { color: C.LINE, width: 1 } });
const RY = TOP + 0.38, RH = 0.64;
s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: LX - 0.08, y: RY - 0.03, w: 11.62, h: RH - 0.04, fill: { color: C.CARD }, line: { color: C.Y, width: 1 }, rectRadius: 0.06, objectName: 'highlight-row-1' });
for (let n = 1; n <= 5; n++) {
  const y = RY + (n - 1) * RH;
  s.addShape(pres.shapes.RECTANGLE, { x: LX, y: y + 0.05, w: 0.26, h: 0.26, fill: { color: n <= 3 ? C.Y : C.MED }, line: { type: 'none' }, objectName: `rank-chip-${n}` });
  s.addText(String(n), tx({ x: LX, y: y + 0.05, w: 0.26, h: 0.26, fontSize: 11, bold: true, fontFace: C.FB, color: C.DARK, align: 'center', valign: 'middle' }));
  s.addText([w(`{{r${n}_theme}}`, { bold: true, fontFace: C.FB, fontSize: 11, breakLine: true }), w(`{{r${n}_drill}}`, { fontSize: 7.5, color: C.GR, breakLine: true }), w(`{{r${n}_align}}`, { fontSize: 8, bold: true, fontFace: C.FB, color: C.Y })],
    tx({ x: LX + 0.34, y: y + 0.0, w: 3.15, h: RH - 0.04, objectName: `row-${n}-theme` }));
  s.addText(`{{r${n}_money}}`, tx({ x: COL.mtxt, y, w: 1.2, h: RH - 0.04, fontSize: 10, valign: 'middle', objectName: `row-${n}-money` }));
  s.addText(`{{r${n}_link}}`, tx({ x: COL.link, y, w: 3.65, h: RH - 0.04, fontSize: 9.5, valign: 'middle', objectName: `row-${n}-link` }));
}
const common = { barDir: 'bar', barGrouping: 'stacked', barGapWidthPct: 180, catAxisOrientation: 'maxMin', catAxisHidden: true, valAxisHidden: true, valAxisMinVal: 0,
  valGridLine: { style: 'none' }, catGridLine: { style: 'none' }, catAxisLineShow: false, showLegend: false, layout: { x: 0, y: 0, w: 1, h: 1 } };
const lab = D.B.map((r) => r[0]);
s.addChart(pres.charts.BAR, [{ name: 'High', labels: lab, values: D.B.map((r) => r[1]) }, { name: 'Medium', labels: lab, values: D.B.map((r) => r[2]) }],
  Object.assign({}, common, { x: COL.score, y: RY - 0.03, w: 1.85, h: 5 * RH - 0.02, chartColors: [C.Y, C.MED], valAxisMaxVal: 5,
    showValue: true, dataLabelPosition: 'inEnd', dataLabelColor: C.DARK, dataLabelFontFace: C.FB, dataLabelFontSize: 10, dataLabelFontBold: true, dataLabelFormatCode: '0.0;-0.0;;', objectName: 'chart-B-score' }));
s.addChart(pres.charts.BAR, [{ name: 'Top 2 by value', labels: lab, values: D.C.map((r) => r[1]) }, { name: 'Other', labels: lab, values: D.C.map((r) => r[2]) }],
  Object.assign({}, common, { x: COL.money, y: RY - 0.03, w: 1.0, h: 5 * RH - 0.02, chartColors: [C.Y, C.MED], showValue: false, objectName: 'chart-C-money' }));
// growth-vs-book strip
const GY = RY + 5 * RH + 0.08;
s.addText([w('Growth sectors vs {{client_short}}’s book', { bold: true, fontFace: C.FB, color: C.Y, fontSize: 11 }), w('   government priority sectors · client exposure (% of book) · water dependency (ENCORE, 0–1)', { color: C.GR, fontSize: 8.5 })],
  tx({ x: LX, y: GY, w: 11.46, h: 0.24, objectName: 'growth-heading' }));
const GW = (11.46 - 3 * 0.12) / 4;
for (let n = 1; n <= 4; n++) {
  const x = LX + (n - 1) * (GW + 0.12), y = GY + 0.28;
  s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y, w: GW, h: 0.62, fill: { color: C.CARD }, line: { color: C.LINE, width: 0.75 }, rectRadius: 0.06, objectName: `growth-${n}` });
  s.addText([w(`{{s2_grow${n}_head}}`, { bold: true, fontFace: C.FB, fontSize: 9.5, color: C.Y, breakLine: true }), w(`{{s2_grow${n}_text}}`, { fontSize: 8.5 })],
    tx({ x: x + 0.1, y: y + 0.04, w: GW - 0.2, h: 0.56, objectName: `growth-${n}-text` }));
}
s.addShape(pres.shapes.ROUNDED_RECTANGLE, { x: LX, y: 6.0, w: 11.46, h: 0.52, fill: { color: C.Y }, line: { type: 'none' }, rectRadius: 0.06, objectName: 'so-what-panel' });
s.addText([w('So what  ', { bold: true, fontFace: C.FB, color: C.DARK }), w('{{s2_sowhat}}', { color: C.DARK })], tx({ x: LX + 0.2, y: 6.02, w: 11.06, h: 0.48, fontSize: 11, valign: 'middle', objectName: 'so-what' }));
s.addText('{{s2_footnote}}', tx({ x: LX, y: 6.58, w: 11.46, h: 0.36, fontSize: 8, italic: true, color: C.GR, objectName: 'footnote' }));
pres.writeFile({ fileName: process.argv[3] }).then(() => console.log('page 3 ok'));
