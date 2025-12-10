# Week 2 Solutions: Time Value of Money

---

## Section 1: Future Value (Basic)

### Exercise 1.1
**Problem:** You invest $2,500 at an annual interest rate of 7%. How much will you have after 4 years?

**Solution:**
- Formula: FV = PV × (1 + r)^t
- FV = $2,500 × (1.07)^4
- FV = $2,500 × 1.3108
- **FV = $3,276.98**

---

### Exercise 1.2
**Problem:** Your savings account has $800 earning 3% annual interest. What will the balance be after 10 years?

**Solution:**
- Formula: FV = PV × (1 + r)^t
- FV = $800 × (1.03)^10
- FV = $800 × 1.3439
- **FV = $1,075.15**

---

### Exercise 1.3
**Problem:** You invest $5,000 at 9% annual interest. How long will it take to double your money?

**Solution:**
- We need FV = 2 × PV = $10,000
- Formula: FV = PV × (1 + r)^t
- $10,000 = $5,000 × (1.09)^t
- 2 = (1.09)^t
- Take natural log: ln(2) = t × ln(1.09)
- t = ln(2) / ln(1.09)
- t = 0.6931 / 0.0862
- **t ≈ 8.04 years**

**Rule of 72 shortcut:** 72 / 9 = 8 years (close approximation!)

---

## Section 2: Present Value (Basic)

### Exercise 2.1
**Problem:** You will receive $10,000 in 5 years. If the discount rate is 6%, what is it worth today?

**Solution:**
- Formula: PV = FV / (1 + r)^t
- PV = $10,000 / (1.06)^5
- PV = $10,000 / 1.3382
- **PV = $7,472.58**

---

### Exercise 2.2
**Problem:** A bond will pay you $1,000 in 8 years. The appropriate discount rate is 4%. What is the present value?

**Solution:**
- Formula: PV = FV / (1 + r)^t
- PV = $1,000 / (1.04)^8
- PV = $1,000 / 1.3686
- **PV = $730.69**

---

### Exercise 2.3
**Problem:** Your uncle promises to give you $15,000 in 3 years. If you could earn 8% on your investments, what is this promise worth today?

**Solution:**
- Formula: PV = FV / (1 + r)^t
- PV = $15,000 / (1.08)^3
- PV = $15,000 / 1.2597
- **PV = $11,907.48**

---

## Section 3: Annuities

### Exercise 3.1
**Problem:** You will receive $500 per year for 6 years, starting one year from now. If the discount rate is 7%, what is the present value of this annuity?

**Solution:**
- Formula: PV = C × [1/r - 1/(r × (1+r)^t)]
- PV = $500 × [1/0.07 - 1/(0.07 × (1.07)^6)]
- PV = $500 × [14.2857 - 1/(0.07 × 1.5007)]
- PV = $500 × [14.2857 - 9.5242]
- PV = $500 × 4.7615
- **PV = $2,380.75**

**Alternative method (easier with calculator):**
- Calculate PV of each payment and sum:
- PV = $500/1.07 + $500/1.07² + ... + $500/1.07⁶
- **PV = $2,380.75**

---

### Exercise 3.2
**Problem:** You plan to save $2,000 per year for 10 years in an account earning 5% annual interest. How much will you have at the end of 10 years (future value)?

**Solution:**
- This is the future value of an annuity
- Each payment grows for a different number of periods
- First payment: $2,000 × (1.05)^9 = $3,103.40
- Second payment: $2,000 × (1.05)^8 = $2,955.62
- ... (continue for all 10 payments)
- Last payment: $2,000 × (1.05)^0 = $2,000

**Using formula:** FV = C × [(1+r)^t - 1] / r
- FV = $2,000 × [(1.05)^10 - 1] / 0.05
- FV = $2,000 × [1.6289 - 1] / 0.05
- FV = $2,000 × 12.5779
- **FV = $25,155.79**

---

### Exercise 3.3
**Problem:** A car loan requires monthly payments of $400 for 5 years (60 months). If the monthly interest rate is 0.5%, what is the present value (loan amount)?

**Solution:**
- Formula: PV = C × [1/r - 1/(r × (1+r)^t)]
- C = $400, r = 0.005, t = 60 months
- PV = $400 × [1/0.005 - 1/(0.005 × (1.005)^60)]
- PV = $400 × [200 - 1/(0.005 × 1.3489)]
- PV = $400 × [200 - 148.26]
- PV = $400 × 51.73
- **PV = $20,691.43**

---

## Section 4: Perpetuities

### Exercise 4.1
**Problem:** A perpetuity pays $300 per year forever. If the discount rate is 6%, what is its present value?

**Solution:**
- Formula: PV = C / r
- PV = $300 / 0.06
- **PV = $5,000**

---

### Exercise 4.2
**Problem:** You want to endow a scholarship that pays $5,000 per year forever. If the expected return is 4%, how much must you donate today?

**Solution:**
- Formula: PV = C / r
- PV = $5,000 / 0.04
- **PV = $125,000**

---

### Exercise 4.3
**Problem:** A preferred stock pays a constant dividend of $2.50 per share forever. If investors require a 5% return, what is the stock worth?

**Solution:**
- Formula: PV = C / r
- PV = $2.50 / 0.05
- **PV = $50.00 per share**

---

## Section 5: Finding Interest Rates

### Exercise 5.1
**Problem:** You invest $3,000 and it grows to $4,500 in 6 years. What was the annual interest rate?

**Solution:**
- Formula: (1 + r)^t = FV / PV
- (1 + r)^6 = $4,500 / $3,000 = 1.5
- 1 + r = (1.5)^(1/6)
- 1 + r = 1.0698
- **r = 6.98% per year**

---

### Exercise 5.2
**Problem:** An investment doubles in 9 years. What is the annual interest rate?

**Solution:**
- Formula: (1 + r)^t = FV / PV
- (1 + r)^9 = 2 (since it doubles)
- 1 + r = 2^(1/9)
- 1 + r = 1.0801
- **r = 8.01% per year**

**Rule of 72 check:** 72 / 9 = 8% ✓

---

### Exercise 5.3
**Problem:** You invest $1,000 today and receive $1,331 in 3 years. What is the annual return?

**Solution:**
- Formula: (1 + r)^t = FV / PV
- (1 + r)^3 = $1,331 / $1,000 = 1.331
- 1 + r = (1.331)^(1/3)
- 1 + r = 1.10
- **r = 10% per year**

---

## Section 6: Real vs Nominal Interest Rates

### Exercise 6.1
**Problem:** The nominal interest rate is 8% and the inflation rate is 2.5%. What is the real interest rate?

**Solution:**
- Exact formula: 1 + real rate = (1 + nominal) / (1 + inflation)
- 1 + real rate = 1.08 / 1.025
- 1 + real rate = 1.0537
- **Real rate = 5.37%**

**Approximation:** 8% - 2.5% = 5.5% (close, slightly overstates)

---

### Exercise 6.2
**Problem:** You expect to earn a real return of 5% per year. If inflation is 3%, what nominal interest rate should you target?

**Solution:**
- Formula: 1 + nominal = (1 + real) × (1 + inflation)
- 1 + nominal = 1.05 × 1.03
- 1 + nominal = 1.0815
- **Nominal rate = 8.15%**

**Approximation:** 5% + 3% = 8% (close)

---

### Exercise 6.3
**Problem:** A bond promises to pay $1,000 in one year. The nominal interest rate is 10% and expected inflation is 4%. What is the present value in real terms?

**Solution:**

**Step 1:** Calculate real interest rate
- 1 + real rate = 1.10 / 1.04 = 1.0577
- Real rate = 5.77%

**Step 2:** Calculate real value of future payment
- In one year, $1,000 nominal = $1,000 / 1.04 = $961.54 in today's purchasing power

**Step 3:** Discount at real rate
- PV (real) = $961.54 / 1.0577
- **PV (real) = $909.09 in today's dollars**

**Alternative method:** Discount nominal cash flow at nominal rate
- PV = $1,000 / 1.10 = $909.09 ✓

---

## Section 7: APR vs EAR

### Exercise 7.1
**Problem:** A bank offers 6% APR with monthly compounding. What is the effective annual rate (EAR)?

**Solution:**
- APR = 6% per year = 0.5% per month
- Formula: EAR = (1 + rate per period)^(periods per year) - 1
- EAR = (1 + 0.06/12)^12 - 1
- EAR = (1.005)^12 - 1
- EAR = 1.0617 - 1
- **EAR = 6.17%**

---

### Exercise 7.2
**Problem:** A credit card charges 2% per month. What are the APR and EAR?

**Solution:**

**APR:**
- APR = 2% × 12 months = **24%**

**EAR:**
- EAR = (1.02)^12 - 1
- EAR = 1.2682 - 1
- **EAR = 26.82%**

Note: The EAR is much higher due to monthly compounding!

---

### Exercise 7.3
**Problem:** Which is better: 8% APR compounded quarterly, or 7.9% APR compounded monthly?

**Solution:**

**Option 1: 8% APR quarterly**
- Rate per quarter = 8% / 4 = 2%
- EAR = (1.02)^4 - 1 = 1.0824 - 1 = **8.24%**

**Option 2: 7.9% APR monthly**
- Rate per month = 7.9% / 12 = 0.6583%
- EAR = (1.006583)^12 - 1 = 1.0819 - 1 = **8.19%**

**Answer:** Option 1 (8% quarterly) has a slightly higher EAR (8.24% vs 8.19%)

---

## Section 8: Mixed Problems (Challenge)

### Exercise 8.1
**Problem:** You need $50,000 in 7 years for a down payment on a house. If you can earn 6% annually, how much must you invest today?

**Solution:**
- Formula: PV = FV / (1 + r)^t
- PV = $50,000 / (1.06)^7
- PV = $50,000 / 1.5036
- **PV = $33,251.73**

---

### Exercise 8.2
**Problem:** A lottery offers you two options:
- Option A: $100,000 today
- Option B: $20,000 per year for 6 years (starting in 1 year)

If the discount rate is 8%, which option is better?

**Solution:**

**Option A:** PV = $100,000 (already in present value)

**Option B:** Calculate PV of 6-year annuity
- Formula: PV = C × [1/r - 1/(r × (1+r)^t)]
- PV = $20,000 × [1/0.08 - 1/(0.08 × (1.08)^6)]
- PV = $20,000 × [12.5 - 1/(0.08 × 1.5869)]
- PV = $20,000 × [12.5 - 7.8738]
- PV = $20,000 × 4.6229
- **PV = $92,458.64**

**Answer:** Take Option A ($100,000 today) - it's worth $7,541.36 more than Option B.

---

### Exercise 8.3
**Problem:** You take out a $200,000 mortgage at 5% annual interest (0.4167% monthly) for 30 years (360 months). What is your monthly payment?

**Solution:**
- This is solving for C in the annuity formula
- Formula: PV = C × [1/r - 1/(r × (1+r)^t)]
- $200,000 = C × [1/0.004167 - 1/(0.004167 × (1.004167)^360)]
- $200,000 = C × [240 - 1/(0.004167 × 4.4677)]
- $200,000 = C × [240 - 53.68]
- $200,000 = C × 186.32
- C = $200,000 / 186.32
- **C = $1,073.64 per month**

---

### Exercise 8.4
**Problem:** A project requires an investment of $100,000 today and will return $25,000 per year for 6 years. If your required return is 10%, should you invest?

**Solution:**
- Calculate PV of returns (6-year annuity)
- Formula: PV = C × [1/r - 1/(r × (1+r)^t)]
- PV = $25,000 × [1/0.10 - 1/(0.10 × (1.10)^6)]
- PV = $25,000 × [10 - 1/(0.10 × 1.7716)]
- PV = $25,000 × [10 - 5.6447]
- PV = $25,000 × 4.3553
- **PV of returns = $108,882.47**

**Decision:**
- PV of returns = $108,882.47
- Cost = $100,000
- Net benefit = $8,882.47
- **Yes, invest!** The present value exceeds the cost.

---

### Exercise 8.5
**Problem:** You invest $5,000 at a nominal rate of 7% per year. Inflation averages 2% per year. How much purchasing power (in today's dollars) will you have in 10 years?

**Solution:**

**Step 1:** Calculate future value at nominal rate
- FV (nominal) = $5,000 × (1.07)^10
- FV (nominal) = $5,000 × 1.9672
- FV (nominal) = $9,835.76

**Step 2:** Calculate real interest rate
- 1 + real = 1.07 / 1.02 = 1.049
- Real rate = 4.9%

**Step 3:** Calculate future value at real rate (purchasing power)
- FV (real) = $5,000 × (1.049)^10
- FV (real) = $5,000 × 1.6144
- **FV (real) = $8,071.94 in today's purchasing power**

**Alternative:** Deflate nominal FV by cumulative inflation
- FV (real) = $9,835.76 / (1.02)^10
- FV (real) = $9,835.76 / 1.2190
- **FV (real) = $8,071.94** ✓

---

## Bonus Challenge

### Exercise 9.1
**Problem:** A perpetuity-due (payments start immediately, not in 1 year) pays $1,000 per year. If the discount rate is 5%, what is its present value?

**Solution:**

**Method 1 - Conceptual:**
- A perpetuity-due = immediate $1,000 + regular perpetuity starting year 2
- Regular perpetuity PV = $1,000 / 0.05 = $20,000
- But this values the perpetuity starting year 1
- Perpetuity-due = $1,000 (today) + $20,000 (PV of future payments)
- **PV = $21,000**

**Method 2 - Formula:**
- PV (perpetuity-due) = C + C/r = C(1 + 1/r)
- PV = $1,000 × (1 + 1/0.05)
- PV = $1,000 × 21
- **PV = $21,000** ✓

---

### Exercise 9.2
**Problem:** You plan to retire in 40 years. You want to withdraw $50,000 per year for 30 years during retirement (first withdrawal at retirement). If the interest rate is 6%, how much do you need to save each year for the next 40 years?

**Solution:**

**Step 1:** Calculate how much you need at retirement
- This is PV of a 30-year annuity at retirement
- PV = $50,000 × [1/0.06 - 1/(0.06 × (1.06)^30)]
- PV = $50,000 × [16.6667 - 1/(0.06 × 5.7435)]
- PV = $50,000 × [16.6667 - 2.9013]
- PV = $50,000 × 13.7648
- **Need $688,238.47 at retirement**

**Step 2:** Calculate annual savings needed (FV of annuity = target)
- FV of annuity = C × [(1+r)^t - 1] / r
- $688,238.47 = C × [(1.06)^40 - 1] / 0.06
- $688,238.47 = C × [10.2857 - 1] / 0.06
- $688,238.47 = C × 154.7620
- C = $688,238.47 / 154.7620
- **C = $4,449.22 per year**

**Answer:** You need to save $4,449.22 per year for 40 years to support $50,000 annual withdrawals for 30 years in retirement.
