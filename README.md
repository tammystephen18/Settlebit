# ⚖️ Settlebit - Legal Settlement Escrow

A Clarity smart contract for managing escrow funds in legal settlements with conditional release mechanisms on the Stacks blockchain.

## 🎯 Overview

Settlebit provides a trustless escrow system for legal settlements where:
- 💰 Funds are held securely until settlement conditions are met
- 🤝 Both parties can agree to release funds
- 👨‍⚖️ An arbitrator can make decisions after deadlines
- 🔒 Emergency refund mechanisms for contract owner

## ✨ Features

- **Settlement Creation**: Create new settlement agreements with defined terms
- **Secure Escrow**: Deposit and hold STX tokens safely
- **Mutual Agreement**: Both parties can agree to settlement terms
- **Arbitration**: Third-party arbitrator can resolve disputes
- **Time-based Resolution**: Automatic deadline enforcement
- **Emergency Controls**: Contract owner emergency refund capability

## 🚀 Usage

### Creating a Settlement

```clarity
(contract-call? .Settlebit create-settlement 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7  ;; defendant
  'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE  ;; arbitrator
  u1000000                                      ;; amount in microSTX
  u1000)                                        ;; deadline block height
```

### Depositing Funds

```clarity
(contract-call? .Settlebit deposit-funds u1)  ;; settlement-id
```

### Agreeing to Settlement

```clarity
(contract-call? .Settlebit agree-to-settlement u1)  ;; settlement-id
```

### Arbitrator Decision

```clarity
(contract-call? .Settlebit arbitrator-decision u1 true)  ;; settlement-id, decision
```

## 📊 Read-Only Functions

- `get-settlement`: Get settlement details
- `get-settlement-funds`: Check deposited amounts
- `get-settlement-counter`: Total settlements created
- `is-settlement-expired`: Check if deadline passed
- `can-arbitrator-decide`: Check arbitrator eligibility
- `get-settlement-status`: Get current settlement status

## 🔧 Settlement Lifecycle

1. **📝 Creation**: Plaintiff creates settlement with defendant and arbitrator
2. **💳 Funding**: Required amount is deposited into escrow
3. **🤝 Agreement**: Both parties can agree to terms
4. **⏰ Deadline**: If no agreement, arbitrator can decide
5. **✅ Resolution**: Funds released to appropriate party

## 🛡️ Security Features

- ✅ Authorization checks for all operations
- ✅ Deadline enforcement
- ✅ Duplicate prevention
- ✅ Fund safety mechanisms
- ✅ Emergency refund capability

## 📋 Settlement Statuses

- `pending`: Created but not funded
- `funded`: Funds deposited, awaiting agreement
- `resolved`: Settlement completed
- `refunded`: Emergency refund executed

## 🔍 Error Codes

- `u100`: Unauthorized access
- `u101`: Settlement not found
- `u102`: Settlement already exists
- `u103`: Insufficient funds
- `u104`: Settlement already resolved
- `u105`: Settlement not active
- `u106`: Invalid party
- `u107`: Deadline passed
- `u108`: Deadline not passed
- `u109`: Already agreed
- `u110`: Invalid amount

## 🏗️ Development

Built with Clarinet for the Stacks blockchain. Ensure you have Clarinet installed for testing and deployment.

```bash
clarinet check
clarinet test
clarinet deploy
```

## ⚠️ Important Notes

- Only the contract owner can execute emergency refunds
- Arbitrators can only decide after deadlines pass
- Funds are automatically released when both parties agree
- All amounts are in microSTX (1 STX = 1,000,000 microSTX)

---

*Built for secure, transparent legal settlement management on Stacks* 🏛️

