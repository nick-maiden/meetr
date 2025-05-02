import React from 'react'
import ReactDOM from 'react-dom/client'
import App from './App.tsx'
import './index.css'

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
)


window.onerror = function (message, source, lineno, colno, error) {
  alert("Error: " + message); // Use alert for visibility on mobile
  console.error("Global Error:", message, source, lineno, colno, error);
};

window.addEventListener("unhandledrejection", function (event) {
  alert("Promise error: " + (event.reason?.message || event.reason));
  console.error("Unhandled promise rejection:", event.reason);
});
