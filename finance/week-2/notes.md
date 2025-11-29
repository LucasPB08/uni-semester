# Week 2: Chapter 5 - Time Value of Money

## Learning Objectives (from textbook)
- LO1: Calculate the future value to which money invested at a given interest rate will grow
- LO2: Calculate the present value of a future payment
- LO3: Calculate present and future values of streams of cash payments
- LO4: Find the interest rate implied by the present or future value
- LO5: Explain the difference between real and nominal cash flows and between real and nominal interest rates
- LO6: Explain how we would compare interest rates quoted over different time intervals

---

## Key Definitions

**Future Value (FV)**: The amount to which money invested at a given interest rate will grow after a certain period.

**Present Value (PV)**: The amount that would need to be invested today to match a future payment value. Equivalently, the value today of $1 received in period t.

**Discount Factor**: The present value of $1 received in the future. Formula: 1/(1 + r)^t

**Discount Rate (r)**: The interest rate used to calculate present value from future cash flows.

**Annuity**: A level stream of cash payments that continues indefinitely for a limited number of years (e.g., mortgage payments, bond coupons).

**Perpetuity**: A level stream of cash payments that continues forever.

**Nominal Interest Rate**: The interest rate that does not account for inflation; measures dollar growth.

**Real Interest Rate**: The interest rate adjusted for inflation; measures growth in purchasing power.

**Annual Percentage Rate (APR)**: Interest rate for short time periods quoted as a rate per year (simple interest, no compounding).

**Effective Annual Rate (EAR)**: The rate that reflects the effects of compound interest; the true annual return.

---

## Formulas

### 1. Future Value (FV)

**Single Payment:**
- FV = PV × (1 + r)^t
- Where: r = interest rate per period, t = number of periods

**Example:** $1 invested at 7% for 2 periods grows to $1 × (1.07)^2 = $1.1449

### 2. Present Value (PV)

**Single Payment:**
- PV = FV / (1 + r)^t
- Or: PV = FV × Discount Factor
- Discount Factor = 1 / (1 + r)^t

**Example:** PV of $1 received in period t = 1/(1 + r)^t

### 3. Annuities (Level Stream for Limited Years)

**Present Value of Annuity:**
- PV = C × [1/r - 1/(r × (1+r)^t)]
- Or using annuity factor tables

**Future Value of Annuity:**
- FV = sum of future value of each cash payment
- Use shortcut formulas for calculations

**Example:** If you receive $100/year for 3 years at 5% interest, calculate PV using annuity formula.

### 4. Perpetuities (Level Stream Forever)

**Present Value of Perpetuity:**
- PV = C / r
- Where C = cash payment per period

**Example:** A perpetuity paying $100/year at 5% has PV = $100/0.05 = $2,000

### 5. Finding Interest Rate from PV/FV (LO4)

**Formula:**
- (1 + r)^t = FV / PV
- Solve for r: r = (FV/PV)^(1/t) - 1

**Example:** If $1,000 grows to $1,500 in 5 years, what's the interest rate?
- r = ($1,500/$1,000)^(1/5) - 1 = 1.5^0.2 - 1 ≈ 8.45%

### 6. Real vs Nominal Interest Rates (LO5)

**Relationship:**
- 1 + nominal rate = (1 + real rate) × (1 + inflation rate)
- Approximation: real rate ≈ nominal rate - inflation rate

**Key Rule:**
- Discount **nominal cash flows** (in current dollars) at **nominal interest rates**
- Discount **real cash flows** (in constant dollars, inflation-adjusted) at **real interest rates**
- **NEVER MIX**: Don't use nominal rates with real cash flows or vice versa

### 7. Comparing Interest Rates (LO6)

**Annual Percentage Rate (APR):**
- APR = rate per period × number of periods per year
- Does NOT account for compound interest
- Example: 1% per month = 12% APR (1% × 12)

**Effective Annual Rate (EAR):**
- EAR = (1 + rate per period)^(periods per year) - 1
- DOES account for compound interest (annualized)
- Example: 1% per month compounded = (1.01)^12 - 1 = 12.68% EAR

**Key Point:** EAR is always ≥ APR when compounding occurs more than once per year.

---

## Key Concepts

### 1. Compound Interest (LO1)
- Money grows exponentially when interest earns interest
- After t periods, $1 grows to $(1 + r)^t
- This is the **future value** of $1
- The power of compounding increases with time

### 2. Discounting and Present Value (LO2)
- To find what a future payment is worth today, we **discount** it
- Discounting is the reverse of compounding
- The discount factor (1/(1+r)^t) tells us the value today of $1 received in period t
- Solving for the interest rate may require trial and error

### 3. Real vs Nominal (LO5)
- **Nominal values**: Measured in current dollars (not adjusted for inflation)
- **Real values**: Measured in constant dollars (purchasing power)
- A dollar today buys more goods than a dollar will buy in the future (inflation)
- Financial managers must be consistent: match nominal cash flows with nominal rates, real with real

### 4. Cash Flow Streams (LO3)
- **Annuities**: Fixed payments for a limited time (e.g., car loans, mortgages)
- **Perpetuities**: Fixed payments forever (rare in practice, useful for valuation)
- Use shortcut formulas instead of calculating each payment individually
- Annuity = Perpetuity - Delayed Perpetuity (conceptually)

### 5. APR vs EAR (LO6)
- Interest rates for short periods are often quoted as annual percentage rates (APR)
- APR uses simple interest multiplication (doesn't compound)
- Effective annual rate (EAR) accounts for compound interest within the year
- When comparing rates with different compounding frequencies, use EAR

---

## Examples

### Example 1: Future Value with Compound Interest
**Problem:** You invest $1,000 at 6% annual interest. What will it be worth in 5 years?

**Solution:**
- FV = $1,000 × (1.06)^5
- FV = $1,000 × 1.3382
- FV = $1,338.20

### Example 2: Present Value of Future Payment
**Problem:** You will receive $5,000 in 3 years. If the discount rate is 8%, what is it worth today?

**Solution:**
- PV = $5,000 / (1.08)^3
- PV = $5,000 / 1.2597
- PV = $3,969.16

### Example 3: Present Value of Perpetuity
**Problem:** A perpetuity pays $200 per year. If the discount rate is 5%, what is its present value?

**Solution:**
- PV = C / r
- PV = $200 / 0.05
- PV = $4,000

### Example 4: Real vs Nominal Interest Rates
**Problem:** Nominal interest rate is 10%, inflation rate is 3%. What is the real interest rate?

**Solution (exact):**
- 1 + real rate = (1 + nominal) / (1 + inflation)
- 1 + real rate = 1.10 / 1.03
- real rate = 1.0680 - 1 = 6.80%

**Solution (approximation):**
- real rate ≈ 10% - 3% = 7% (close, but slightly overstates)

### Example 5: APR vs EAR
**Problem:** A credit card charges 1.5% per month. What are the APR and EAR?

**Solution:**
- APR = 1.5% × 12 = 18%
- EAR = (1.015)^12 - 1 = 1.1956 - 1 = 19.56%

The EAR is higher because of monthly compounding.

### Example 6: Finding the Interest Rate
**Problem:** An investment of $2,000 grows to $3,000 in 6 years. What was the annual interest rate?

**Solution:**
- (1 + r)^6 = $3,000 / $2,000 = 1.5
- r = (1.5)^(1/6) - 1
- r = 1.0698 - 1
- r = 6.98% per year

---

## Summary Points (from textbook)

1. An investment of $1 earning interest at rate r will increase in value each period by the factor (1 + r). After t periods, its value will grow to $1 × (1 + r)^t - this is the **future value** of $1 with compound interest.

2. The **present value** of a cash payment is the amount you would need to invest today to match that future payment value. To calculate the present value, divide the cash payment by (1 + r)^t, equivalently multiply by the **discount factor** 1/(1 + r)^t. The discount factor measures the value today of $1 received in period t.

3. Be careful to distinguish the **nominal interest rate** and the **real interest rate** - the rate at which the real value of the investment grows. Discount nominal cash flows (measured in current dollars) at nominal interest rates. Discount **real cash flows** (measured in constant dollars) at real interest rates. **Never mix nominal and real.**

4. The **present value** equals the discounted value of one or more future cash flows. Shortcut formulas make the calculations for perpetuities and annuities easy.

5. A level stream of cash payments that continues indefinitely is a **perpetuity**. One that continues for a limited number of years is an **annuity**.

6. Interest rates for short time periods are often quoted as **annual percentage rates (APR)**, which annualize using simple interest (the rate per period × number of periods in a year). The **effective annual rate** annualizes using compound interest. It equals the rate of compound interest, that is, they annualize assuming simple multiplication. The effective annual rate equals the rate per period compounded over the number of periods in a year.

---

## Practice Tips

- Always identify whether you're calculating FV or PV before starting
- Draw a timeline to visualize cash flows
- Be consistent with time periods (if r is annual, t must be in years)
- When using real vs nominal, always match: real cash flows → real rate, nominal → nominal
- For annuities/perpetuities, verify that payments are truly level (same amount each period)
- Use a financial calculator or spreadsheet for complex calculations
- When finding interest rates, you may need to use logarithms or trial-and-error
