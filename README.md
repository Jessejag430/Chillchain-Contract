# 🧊 Chillchain - Cold Chain Verification Protocol

> Temperature monitoring meets blockchain technology for transparent supply chain verification

## 📋 Overview

Chillchain is a **Cold Chain Verification Protocol** built on the Stacks blockchain using Clarity smart contracts. It combines **temperature logging** with **NFT ownership** to create an immutable record of product conditions during transportation and storage.

## ✨ Key Features

- 🎫 **NFT-Based Ownership**: Each shipment is represented as a unique NFT
- 🌡️ **Temperature Monitoring**: Real-time temperature logging with violation tracking  
- 🔐 **Authorized Sensors**: Only verified sensors can submit temperature data
- 📊 **Compliance Tracking**: Automatic detection of temperature threshold violations
- 🚚 **Shipment Lifecycle**: Complete tracking from origin to destination
- 🔄 **Transferable Ownership**: NFT-based shipment ownership transfers

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd chillchain
clarinet check
```

## 📖 Usage Guide

### 1. 🆕 Create a New Shipment

```clarity
(contract-call? .Chillchain create-shipment 
  "Fresh Salmon" 
  "Seattle Port" 
  "Tokyo Market" 
  -2  ;; min temp (°C)
  4)  ;; max temp (°C)
```

### 2. 🔧 Authorize Temperature Sensors

```clarity
(contract-call? .Chillchain authorize-sensor "SENSOR-001")
```

### 3. 📝 Log Temperature Data

```clarity
(contract-call? .Chillchain log-temperature 
  u1           ;; shipment-id
  2            ;; temperature (°C)
  "Port of LA"  ;; location
  "SENSOR-001") ;; sensor-id
```

### 4. ✅ Complete Shipment

```clarity
(contract-call? .Chillchain complete-shipment u1)
```

### 5. 🔄 Transfer Ownership

```clarity
(contract-call? .Chillchain transfer-shipment u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

## 🔍 Query Functions

### Get Shipment Details
```clarity
(contract-call? .Chillchain get-shipment u1)
```

### Check Temperature Compliance
```clarity
(contract-call? .Chillchain is-temperature-compliant u1)
```

### View Temperature Logs
```clarity
(contract-call? .Chillchain get-temperature-log u1 u0)
```

### Get Violation Count
```clarity
(contract-call? .Chillchain get-shipment-violations u1)
```

## 🏗️ Contract Architecture

### Data Structures

- **Shipments**: Core shipment data with temperature thresholds
- **Temperature Logs**: Timestamped temperature readings with location data
- **Authorized Sensors**: Whitelist of verified temperature sensors
- **NFT Ownership**: Each shipment is a transferable NFT

### Key Functions

| Function | Purpose |
|----------|---------|
| `create-shipment` | Initialize new cold chain tracking |
| `log-temperature` | Record temperature readings |
| `complete-shipment` | Finalize shipment delivery |
| `authorize-sensor` | Add trusted temperature sensors |

## 🎯 Use Cases

- 🐟 **Food & Beverage**: Track perishable goods from farm to table
- 💊 **Pharmaceuticals**: Ensure vaccine and medicine cold chain integrity  
- 🧬 **Biotechnology**: Monitor sensitive biological samples
- 🌸 **Floriculture**: Maintain optimal conditions for flower shipments

## 🔒 Security Features

- ✅ Only authorized sensors can submit temperature data
- ✅ NFT ownership controls shipment management
- ✅ Immutable temperature violation records
- ✅ Contract owner controls sensor authorization

## 🧪 Testing

```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License.

---

**Built with ❄️ by the Chillchain team**

