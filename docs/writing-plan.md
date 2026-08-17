# Writing plan: MCMC convergence graph / pairwise $\hat{R}$

Outline organised from the whiteboard session (Jul 2026). Space budgets and figure cues are retained as drafting targets.

---

## 3 Methods / Implementation block

### 3.1 Implementation — R package (MCMC conv graph)

**Space:** ~1/8–1/4 page

**Purpose.** Introduce the R package as the practical realisation of the pairwise $\hat{R}$ / MCMC convergence graph pipeline.

**Figure — Fig. 3: R package MCMC conv graph**

TikZ source: [`docs/fig_package_workflow.tex`](fig_package_workflow.tex).  
Style matched to the theory pipeline figure (`fig_pipeline.tex`): outer box, vertical dividers, grayscale chain nodes, bold `(a)/(b)/(c)`.

| Panel | Title | Content |
| --- | --- | --- |
| (a) | 1. Input | `stanfit` **or** draws array $I\times C\times P$ (axes labelled) |
| (b) | 2. Calculation | five stages: standardise → pairwise $\hat{R}_{ij}$ → matrices → threshold $\rho$ → combine |
| (c) | 3. Numerical and graphical output | **Numerical** (matches `print.pairwiserhat`): (1) per-parameter matrices, (2) top-10 $\hat{R}_{ij}$, (3) summary; **Graphical**: single $G_{\rho}$ |

**Drafting notes.**

- Keep this subsection short: package role + Fig. 3 workflow only.
- Defer full walk-through of user commands to §3.3.
- No concrete floats in Fig. 3; schematic matrices / graphs only.
- Sara-style caption for the theory pipeline figure: [`docs/fig_pipeline_caption_sara.tex`](fig_pipeline_caption_sara.tex).

---

### 3.2 Experimental setup

**Space:** ~1/2 page

**Purpose.** Define the controlled cases used later in §4.

**Structure.**

1. We consider **4 cases / distributions**, ordered by **increasing complexity** (1 → 4).
2. Include **plots of generated data** for each case.
3. Summarise the four cases in a compact table or short list (“Summarise 4”).

**Drafting notes.**

- Setup only: what is sampled, dimensions, chain configuration — not results.
- Cross-reference §4 for findings.

---

### 3.3 Using the R package

**Purpose.** Show how a reader applies the package on the setups from §3.2.

**Content to cover.**

- End-to-end use of the package (entry point → numerical summaries → graph).
- **On scaling:** how the method scales to high-dimensional $\theta$ and to many chains (callback to theory).

**Drafting notes.**

- Follow the verbal → maths → code → output → gloss cycle (Sara / Cornell–Suprunenko style).
- Keep Implementation-facing prose method-focused; concrete floats belong in §4.

---

## 4 Experiments

### 4.1 Experiment a — controlled distributions

**Goal.** Strong figure; establish basic behaviour against classic $\hat{R}$.

**Setup.** We sample from controlled distributions (CONTROL).

**Claims to demonstrate.**

1. Pairwise $\hat{R}$ can detect multimodality in the data.
2. More sensitive than classic $\hat{R}$ at detecting different geometry (e.g. funnel).
3. In the unimodal case, agrees with classic $\hat{R}$ (and can show greater sensitivity).

**Figure / table cue.**

- Comparison table with columns such as: Param | Classic | Master | RT | cmp  
- Include a row for the LINE case (and related geometries as needed).

---

### 4.2 Experiment b — linear regression / inferred posteriors

**Setup.** Linear regression model $y = ax + b$ (and mixture extensions).

**Mixture sketch (board).**

$$
y \mid x_i \sim \mathcal{N}(a_i x_i + b_i, \sigma^2) + \cdots
$$

**Design grid (vary parameters / weights).**

| Variant | Description |
| --- | --- |
| vary $a$ | different slopes |
| vary $b$ | different intercepts |
| vary $a$ & $b$ | both free |
| vary $a$ & $b$, different weights | mixture with unequal weights |

**Named case ladder (board).**

1. Gauss base  
2. Sleep  
3. PKPD  

**Note.** Clusters should be named with **minimal names**.

**Claims to demonstrate.**

1. The method still works when we **infer the posterior via a model**, not only when we sample known distributions as in §4.1.
2. Chain grouping is **not** the same as the number of modes in the posterior (sampler-dependent). We want a suite that is **generalizable across MCMC samplers**.

---

### 4.3 Comparison — PKPD / real data

**Setup.** We sample from a model on a **real data set** (PKPD).

**Claim.**

- The MCMC convergence graph can detect multimodality arising from **model structure** (e.g. label switching).

---

### 4.4 (or 5.1) How to deal with graph outputs

**Purpose.** Practical guidance when the graph is hard to interpret or act on.

**Issues / responses (board).**

- Too few chains  
- Too many domains  
- Accept multimodality  
- Change the model  
- Scaling to high dimensions  

*(Decide later whether this sits as §4.4 under Experiments or as §5.1 opening Discussion.)*

---

## 5 Discussion

**Guiding questions.**

1. What to do if there is an **isolated chain** → typically increase the number of chains (or re-run).
2. What to do when multimodality is considered jointly with **data** and **model structure** (synergies / trade-offs, ±).

**Narrative arc (four beats).**

1. We introduce the theory.  
2. We show that … (results-based synthesis / combine §4 findings).  
3. We make the method accessible via the R package (why it is flexible; who can use it).  
4. We underline the contribution: a diagnostic tool for multimodality / chain disagreement (callback to the discussion paper and to the intro: multimodality, importance of the point load).

---

## Open decisions

- [ ] Exact length of §3.1 (1/8 vs 1/4 page) once Fig. 3 is drafted.
- [ ] Final list of the four cases in §3.2 (map to existing experiment scripts).
- [ ] Whether “How to deal with graph outputs” is §4.4 or §5.1.
- [ ] Naming of §4 tables (Classic / Master / RT / cmp columns — confirm definitions).
- [ ] Which strong figure anchors §4.1.
- [ ] Scope of Sleep vs PKPD in §4.2 vs §4.3 (avoid duplication).

---

## Suggested figure / table inventory

| ID | Section | Description |
| --- | --- | --- |
| Fig. 3 | §3.1 | Package workflow: samples → matrix → graph |
| — | §3.2 | Plots of the four generated-data cases |
| — | §4.1 | Strong CONTROL figure + Param / Classic / Master / RT / cmp table |
| — | §4.2 | Linear / mixture variants (vary $a$, $b$, weights) |
| — | §4.3 | PKPD real-data convergence graph (label switching) |
