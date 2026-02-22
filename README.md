# Sociolinguistics

Sociolinguistic research on Korean spoken and written language variation across two projects.

---

## Project 1: Vowel Lengthening with Chat Data

**Key word**: *Style shift*

**Summary**: Investigates stylistic variation and vowel lengthening in written chat/messaging data. Examines how speakers mark vowel lengthening (e.g., tilde ~) in text and relates this to speaker identity and social network context. Uses Press member network chat data with manually collected messaging; members are ranked by network counts, active membership, and network type counts.

**Data information and processing steps**:
- Data: Press member network chat data; manually collected written messaging; 12 members (HJ, MC, J, SM, ME, JR, SH, UM, OY, EM, SY, DY); network metrics per member.
- Processing: Reshape raw array to DataFrame with columns (total network counts, active member, network type counts, sum, rank); index by member ID; identify lengthening markers (e.g., tilde ~) in chat messages; aggregate metrics per member for ranking.

**Core analysis and technique**:
- Network ranking: extraction and aggregation of network metrics (total network counts, network type counts, active member, sum, rank) per member.
- Style variation: identification of lengthening markers in chat messages and comparison across members.
- Technique: descriptive statistics, network metrics aggregation, tabular comparison.

**Core libraries**: pandas, numpy, seaborn, matplotlib, collections, re, os

---

## Project 2: Intonational Phrase Length and Vowel Lengthening in Spoken Transcriptions

**Key word**: *Intonational phrase*

**Summary**: Two analyses on the NIKL (National Institute of Korean Language) Dialogue corpus 2023. (1) **IP length**: Relationship between speaker sex and intonational phrase length (token count, duration per phrase) in spoken Korean dialogue; modeled with SVR. (2) **Vowel lengthening in transcriptions**: How vowel lengthening and phonetic variants (e.g., tilde ~, spelling variants) are marked in NIKL transcriptions; compares *original_form* vs. *form* to detect deviation.

**Data information and processing steps**:
- Data: NIKL Dialogue 2023 corpus; JSON files (~1974 files); each file: `document`, `metadata`, `utterance`; speaker metadata (id, sex, age, occupation, birthplace, residence, education); utterance fields: `form`, `original_form`, `speaker_id`, `start`, `end`; 2–4 speakers per dialogue.
- Processing: Load JSON; extract speakers and map sex (여성/남성 to M/F); extract utterances; compute *token_count*, *syll_count*, *duration*; derive *audience_sex* and *sex_combi* for dyadic vs. group contexts; one-hot encode sex, audience_sex, sex_combi; for deviation analysis: *extract_speech* per speaker; compare *original_form* vs. *form* to build deviation pairs.

**Core analysis and technique**:
- IP length modeling: SVR with RBF kernel; GridSearchCV for hyperparameters (C, epsilon); negative MSE scoring; StandardScaler for feature scaling.
- Deviation detection: *extract_speech* extracts per-speaker utterances; pairs where *original_form* != *form* capture lengthening markers and spelling variants.
- EDA: mean, variance of token count and IP length; distribution analysis per speaker and per dialogue.
- Technique: Support Vector Regression, grid search, deviation comparison, descriptive statistics.

**Core libraries**: pandas, numpy, seaborn, matplotlib, json, os, collections, scikit-learn (SVR, GridSearchCV, StandardScaler, mean_squared_error, train_test_split)
