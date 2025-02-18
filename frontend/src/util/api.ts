import axios from 'axios';

const BACKEND_URL = import.meta.env.REACT_APP_BACKEND_URL
const BACKEND_PORT = import.meta.env.REACT_APP_BACKEND_PORT
const BASE_URL = `${BACKEND_URL}:${BACKEND_PORT}`;

const defaultHeaders = {
  'Content-Type': 'application/json'
};

export const postRequest = (path: string, data: any) => {
  return axios.post(`${BASE_URL}${path}`, data, {
    headers: defaultHeaders
  });
};

export const getRequest = (path: string) => {
  return axios.get(`${BASE_URL}${path}`, {
    headers: defaultHeaders
  });
};

export const deleteRequest = (path: string) => {
  return axios.delete(`${BASE_URL}${path}`, {
    headers: defaultHeaders
  });
};

export const putRequest = (path: string, data: any) => {
  return axios.put(`${BASE_URL}${path}`, data, {
    headers: defaultHeaders
  });
};

