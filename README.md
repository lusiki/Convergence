

<p align="center">
<img src="./photo.jpg" width="750" title="hover text">
</p>


# OpenSource and Reproducible Analysis of Economic Convergence Clubs for Croatia

## Overview

This repository contains the resources and data for the research paper titled **"OpenSource and Reproducible Analysis of Economic Convergence Clubs for Croatia."** The study investigates the income convergence of Croatia from 2000 to 2024 across four groups of countries: EU15, NMS8, NMS12, and SE4. Utilizing time series methodology, specifically fractional integration, the empirical analysis examines Croatia's membership in different economic convergence clubs within the European Union.

## Key Findings

- **Convergence with EU15 and SE4:**
  - **EU15:** Croatia exhibits income convergence with EU15 countries, supported by significant fractional integration parameters ($d$) ranging from 0.5643*** to 0.8986***. These values indicate a mean-reverting process, albeit a slow convergence.
  - **SE4:** Similar convergence is observed with SE4 countries, with $d$ values between 0.8852*** and 0.9516***, suggesting that income differences are reducing over time, though the process remains gradual.

- **Lack of Convergence with NMS8 and NMS12:**
  - **NMS8 and NMS12:** The analysis does not support convergence between Croatia and the NMS8 and NMS12 groups. The estimated $d$ parameters exceed 1 (1.4347*** to 1.5002*** for NMS8 and 1.2523*** to 1.5002*** for NMS12), indicating persistent income differences and the absence of convergence.

- **Robustness of Results:**
  - The findings are robust across various estimators and bandwidth selections, particularly highlighting permanent income differences between Croatia and NMS8/NMS12.

## Methodology

The study employs the Autoregressive Distributed Lag (ADF) test and the Geweke-Porter-Hudak (GPH) test to assess the stationarity and fractional integration of income differentials between Croatia and the selected country groups.

### ADF Test Results

The ADF tests evaluate absolute, conditional, and deterministic convergence by testing stationarity around zero difference, non-zero difference, and deterministic trends, respectively.

**Table 2. ADF Test Results**

| **Group Name**   | **Without Const/Trend** | **With Const** | **With Const/Trend** |
|------------------|-------------------------|-----------------|-----------------------|
| RH-NMS8          | -0.3342                 | -1.2816         | -1.2669               |
| RH-NMS12         | -0.2698                 | -1.0464         | -1.3638               |
| RH-SE4           | -4.1785***              | -0.7209         | -2.6062*              |
| RH-EU15          | -2.97**                 | -1.2619         | -1.9528               |

*** p < 0.01, ** p < 0.05, * p < 0.1

### GPH Test Results

The GPH tests examine the fractional integration parameters ($d$) across different bandwidths to determine the presence and nature of convergence.

**Table 3. GPH Test Results for Various Bandwidths**

| **Group Name**     | **GPH $d$ (0.4)**        | **GPH $d$ (0.5)**        | **GPH $d$ (0.6)**        | **GPH $d$ (0.7)**        | **GPH $d$ (0.8)**        | **GPH $d$ (0.9)**        |
|--------------------|--------------------------|--------------------------|--------------------------|--------------------------|--------------------------|--------------------------|
| **Croatia vs NMS8** | 1.4347***                | 1.1921***                | 1.286***                 | 1.1137***                | 1.1511***                | 1.1446***                |
| **Croatia vs NMS12**| 1.5002***                | 1.2523***                | 1.2915***                | 1.1047***                | 1.1534***                | 1.1048***                |
| **Croatia vs SE4**  | 0.8852***                | 0.8712***                | 0.8867***                | 0.9279***                | 0.9272***                | 0.9516***                |
| **Croatia vs EU15** | 0.5643***                | 0.6541***                | 0.7256***                | 0.8405***                | 0.8554***                | 0.8986***                |

*** p < 0.01

## Summary of Results

### Absolute Convergence

Absolute convergence is tested without including a constant and trend in the specification. The ADF test assesses the stationarity around zero income difference.

- **NMS8 and NMS12:**
  - **Test Statistics:** -0.3342 (RH-NMS8) and -0.2698 (RH-NMS12)
  - **Significance:** Not significant
  - **Interpretation:** The null hypothesis of a unit root cannot be rejected, indicating no absolute convergence between Croatia and NMS8/NMS12.

- **SE4 and EU15:**
  - **Test Statistics:** -4.1785*** (RH-SE4) and -2.97** (RH-EU15)
  - **Significance:** Significant at 1% and 5% levels, respectively
  - **Interpretation:** Significant evidence of absolute convergence between Croatia and SE4/EU15 groups.

### Conditional Convergence

Conditional convergence is examined by including a constant in the specification, allowing for non-zero income differences.

- **All Groups (NMS8, NMS12, SE4, EU15):**
  - **Test Statistics:** Ranging from -1.2816 to -1.2619
  - **Significance:** Not significant
  - **Interpretation:** No evidence of conditional convergence between Croatia and any of the analyzed country groups.

### Deterministic Trend Convergence

This convergence is assessed by including both a constant and a trend in the specification, testing for convergence towards a common growth path.

- **NMS8 and NMS12:**
  - **Test Statistics:** -1.2669 (RH-NMS8) and -1.3638 (RH-NMS12)
  - **Significance:** Not significant
  - **Interpretation:** No convergence towards a common trend with NMS8 and NMS12.

- **SE4:**
  - **Test Statistics:** -2.6062*
  - **Significance:** Significant at 10%
  - **Interpretation:** Weak evidence of convergence towards a common trend with SE4.

- **EU15:**
  - **Test Statistics:** -1.9528
  - **Significance:** Not significant
  - **Interpretation:** No convergence towards a common trend with EU15.

## SAŽETAK

Rad analizira dohodovnu konvergenciju Hrvatske u razdoblju od 2000. do 2024. godine prema četiri skupine zemalja: EU15, NMS8, NMS12 i SE4. Korištenjem metodologije vremenskih serija i frakcijske integracije, istražuje se pripadnost Hrvatske različitim konvergencijskim klubovima unutar Europske unije. Rezultati pokazuju da Hrvatska konvergira prema dohodovnim razinama EU15 i SE4, s procijenjenim parametrima frakcijske integracije od 0,87 i 0,95. To ukazuje na spor, ali postojan konvergencijski proces. Međutim, konvergencija prema NMS8 i NMS12 nije potvrđena, s parametrima iznad 1, što sugerira trajne dohodovne razlike. Ovi nalazi impliciraju da se Hrvatska ekonomski približava starim članicama EU i zemljama južne Europe, ali ne i novim zemljama članicama.

## Zaključak

Rad je analizirao dohodovnu konvergenciju Hrvatske s različitim skupinama zemalja Europske unije koristeći analizu vremenskih serija i metode frakcijske integracije. Dok je prethodna literatura pokazala postojanje dohodovne konvergencije za neke, ali ne sve posttranzicijske europske zemlje prema prosjeku EU, ovaj rad proširio je to područje fokusirajući se na konvergencijske obrasce specifične za Hrvatsku. Dodatno, pitanje dohodovne konvergencije smješteno je u širi kontekst europskih integracijskih procesa kroz analizu međusobne konvergencije između Hrvatske i prosjeka dohotka četiri grupe zemalja: EU15, NMS8, NMS12 i SE4. Time je omogućeno zaključivanje o smjeru ekonomske i institucionalne konvergencije unutar europskih integracija te o pripadnosti Hrvatske određenim konvergencijskim klubovima.

Rezultati testova frakcijske integracije dohodovnog diferencijala između Hrvatske i odabranih skupina zemalja pokazali su da konvergencija postoji između Hrvatske i EU15 te Hrvatske i SE4, dok ona nije potvrđena između Hrvatske i NMS8 te NMS12. Svi rezultati su robusni s obzirom na različite procjenitelje i različite izbore širine pojasa. Nalazi impliciraju da je konvergencija Hrvatske prema prosjeku EU15 i SE4 spor proces, ali nešto brži prema SE4. Također se nameće i zaključak da Hrvatska ne pripada konvergencijskom klubu novih članica (NMS8 i NMS12), već konvergencijskim klubovima EU15 i SE4, pri čemu je konvergencija prema SE4 robusnija i brža nego prema EU15.

Ovi nalazi imaju implikacije za europske integracijske procese. Unatoč kasnijem pristupanju Europskoj uniji i primjeni mjera institucionalnog, pravnog i ekonomskog usklađivanja s novim članicama, Hrvatska ne pokazuje dohodovnu konvergenciju s grupama NMS8 i NMS12. To sugerira da institucionalna i pravna integracija ne rezultiraju nužno i ekonomskom konvergencijom. Čini se da Hrvatska ekonomski konvergira prema zemljama EU15 i SE4, koje karakteriziraju sporiji ili negativan gospodarski rast. Potvrda konvergencije prema SE4 ukazuje na pridruživanje Hrvatske konvergencijskom klubu južne Europe i općenito ukazuje na potrebu za politikama koje će potaknuti brži gospodarski rast i razvoj. Ovi nalazi naglašavaju važnost uvažavanja dugoročnih ovisnosti u analizi dohodovne konvergencije. Perzistentnost koju sugeriraju ACF grafikoni ukazuje na to da šokovi u dohodovnim diferencijalima između Hrvatske i odgovarajućih skupina zemalja imaju trajne učinke, te se frakcijska integracija nameće kao prikladna metoda za uvažavanje ovakve konvergencijske dinamike.

## Resources

- **Working Paper (HTML):** [Link](https://raw.githack.com/lusiki/Convergence/main/Paper.html)
- **Codes:** [Codes.R](https://github.com/lusiki/Convergence/blob/main/Codes.R)
- **Data:** Auto-generated from within the `Paper.Rmd` file.

## Reproduction

To reproduce the analysis, follow these steps:

1. **Clone the Repository:**
   ```bash
   git clone https://github.com/lusiki/Convergence.git
   cd Convergence
