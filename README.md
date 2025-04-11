# RepGig - Decentralized Reputation System for Gig Workers

RepGig is a blockchain-based reputation system that enables gig workers to build and maintain verifiable reputation scores through non-transferable tokens.

## Features

- Worker registration
- Job completion tracking
- Peer endorsements with scoring
- Non-transferable reputation tokens
- Transparent reputation scoring

## Smart Contract Functions

### For Workers
- `register-worker`: Register as a new worker
- `get-worker-profile`: View worker's profile data
- `get-reputation-score`: Get worker's average reputation score

### For Employers/Peers
- `complete-job`: Mark a job as completed for a worker
- `endorse-worker`: Endorse a worker with a score (0-5)
- `get-token-data`: View reputation token metadata
- `get-owner`: Check token ownership
- `get-last-token-id`: Get the latest token ID

## Usage

1. Workers register using `register-worker`
2. After job completion, employers call `complete-job`
3. Peers can endorse workers using `endorse-worker`
4. Anyone can view reputation scores using `get-reputation-score`

## Reputation Scoring

- Scores range from 0 to 5
- Each endorsement mints a new reputation token
- Final score is the average of all endorsements
```
