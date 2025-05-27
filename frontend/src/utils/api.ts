import axios from 'axios';

const BACKEND_URL = import.meta.env.VITE_BACKEND_URL

const defaultHeaders = {
  'Content-Type': 'application/json'
};

export const postRequest = (path: string, data: any) => {
  return axios.post(`${BACKEND_URL}${path}`, data, {
    headers: defaultHeaders
  });
};

export const getRequest = (path: string) => {
  return axios.get(`${BACKEND_URL}${path}`, {
    headers: defaultHeaders
  });
};

export const deleteRequest = (path: string) => {
  return axios.delete(`${BACKEND_URL}${path}`, {
    headers: defaultHeaders
  });
};

export const putRequest = (path: string, data: any) => {
  return axios.put(`${BACKEND_URL}${path}`, data, {
    headers: defaultHeaders
  });
};

