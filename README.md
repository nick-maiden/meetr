# 📅 meetr
A web app that makes organising a time to meet easy. Create an event, share the link with attendees and work out the best time to meet based on everyone's availability.

Inspired by [when2meet](https://www.when2meet.com), meetr aims to preserve this simplicity but provide a more aesthetic, functional and responsive UI.

You can find meetr [here](https://www.meetr.app).


## 🚀 Installation

Follow these steps to setup meetr for hosting locally.

### 1. Clone the Repository
```bash
git clone https://github.com/nick-maiden/meetr.git
cd meetr
```

### 2. Setup backend server
```bash
cd backend

# port number can be changed, just ensure it matches with frontend
echo "PORT=8080" > .env
echo "HOST=127.0.0.1" >> .env
echo "ALLOWED_ORIGINS=http://localhost:5173" >> .env
echo "LOG_LEVEL=Development" >> .env
```

### 3. Setup frontend
```bash
cd ../frontend
echo "VITE_BACKEND_URL=http://localhost:8080" > .env
```

## ▶️ Running meetr

Follow these steps to run meetr locally.

### 1. Run backend server
```bash
cd backend
export $(grep -v '^#' .env | xargs)
stack run
```

### 2. Run frontend
```bash
cd ../frontend
npm run dev
```

Navigate to http://localhost:5173

## 🤝 Contributing

All contributions are encouraged! I could really do with the help 🫠

### Found a bug or have an idea for a new feature?
Feel free to [open an issue](https://github.com/nick-maiden/meetr/issues).

### To contribute code:

1. **Fork the repository**
2. **Create a new branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```
3. **Make your changes**
4. **Commit and push**  
   ```bash
   git commit -m "Add your message"
   git push origin feature/your-feature-name
   ```
5. **Open a pull request**


## ⚖️ License
meetr is licensed under the [GNU GPLv3 License](LICENSE).

