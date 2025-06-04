# Laddersave

# 🪜 LadderSave - Incentivized Savings Contract

## 📋 Overview

LadderSave is a Clarity smart contract that gamifies the savings process by creating milestone-based savings goals. Users can create savings "ladders" with multiple milestones, earning rewards as they progress toward their financial targets.

## ✨ Features

- 🎯 **Milestone-Based Savings**: Break large savings goals into achievable milestones
- 🏆 **Progressive Rewards**: Earn increasing rewards for each milestone reached
- 💰 **Completion Bonuses**: Get extra rewards for completing entire savings ladders
- 📈 **Flexible Ladder Creation**: Choose 3-20 milestones per savings goal
- ⚡ **Emergency Withdrawal**: Access funds early with a 10% penalty
- 🔒 **Secure Storage**: Funds locked in contract until milestones are reached

## 🚀 Getting Started

### Prerequisites

- Clarinet installed
- Stacks wallet with STX tokens

### Installation

```bash
clarinet new laddersave-project
cd laddersave-project
```

Copy the contract code into `contracts/Laddersave.clar`

## 📖 Usage Guide

### Creating a Savings Ladder

```clarity
(contract-call? .Laddersave create-savings-ladder u1000000 u10)
```

Creates a ladder with 1,000,000 microSTX target (1 STX) divided into 10 milestones.

### Making Deposits

```clarity
(contract-call? .Laddersave deposit-to-ladder u1 u100000)
```

Deposits 100,000 microSTX (0.1 STX) to ladder ID 1.

### Claiming Milestone Rewards

```clarity
(contract-call? .Laddersave claim-milestone-reward u1 u1)
```

Claims reward for reaching milestone 1 of ladder 1.

### Withdrawing Completed Ladder

```clarity
(contract-call? .Laddersave withdraw-completed-ladder u1)
```

Withdraws principal + completion bonus after reaching target amount.

## 🔍 Read-Only Functions

- `get-ladder-details`: View ladder information
- `get-ladder-progress`: Check progress and available milestones
- `get-milestone-status`: See if milestone reward was claimed
- `estimate-total-rewards`: Calculate potential rewards before creating ladder

## 💡 Reward System

### Milestone Rewards
- Base reward: 5% of milestone amount
- Multiplier bonus based on ladder complexity:
  - 3-5 milestones: 1x multiplier
  - 6-10 milestones: 2x multiplier  
  - 11-15 milestones: 3x multiplier
  - 16-20 milestones: 4x multiplier
- Progressive bonus: Increases with each milestone

### Completion Bonus
- Base: 2% of target amount
- Milestone bonus: 1000 microSTX per milestone
- Multiplier bonus applied

## ⚠️ Important Notes

- Rewards are paid from the contract's reward pool
- Contract owner can fund the reward pool using `fund-rewards-pool`
- Emergency withdrawals incur a 10% penalty
- Penalties are added back to the reward pool
- Each user can create multiple savings ladders

## 🛠️ Development

### Testing

```bash
clarinet test
```

### Deployment

```bash
clarinet deploy
```

## 📊 Example Scenarios

### Small Saver (5 STX goal, 5 milestones)
- Milestone amount: 1 STX each
- Milestone rewards: ~0.05 STX each
- Completion bonus: ~0.15 STX
- Total rewards: ~0.4 STX

### Ambitious Saver (50 STX goal, 20 milestones)  
- Milestone amount: 2.5 STX each
- Higher multiplier rewards
- Substantial completion bonus
- Total rewards: ~5+ STX

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your