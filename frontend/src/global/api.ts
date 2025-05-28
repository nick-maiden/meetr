import axios from 'axios';
import { EventId, EventInput, NewAvailabilityInput, UpdateAvailabilityInput, UserId } from './types';

const BACKEND_URL = import.meta.env.VITE_BACKEND_URL

const defaultHeaders = {
  'Content-Type': 'application/json'
};

const post = (path: string, data: any) => {
  return axios.post(`${BACKEND_URL}${path}`, data, {
    headers: defaultHeaders
  });
};

const get = (path: string) => {
  return axios.get(`${BACKEND_URL}${path}`, {
    headers: defaultHeaders
  });
};

const put = (path: string, data: any) => {
  return axios.put(`${BACKEND_URL}${path}`, data, {
    headers: defaultHeaders
  });
};

/* get */
export const getEvent = (eventId: EventId) => get(`/events/${eventId}`);

/* post/put */
export const createEvent = (newEvent: EventInput) => post('/events', newEvent);

export const addAvailability = (
  eventId: EventId,
  input: NewAvailabilityInput
) => post(`/events/${eventId}/availability`, input);

export const updateAvailability = (
  eventId: EventId,
  userId: UserId,
  input: UpdateAvailabilityInput
) => put(`/events/${eventId}/availability/${userId}`, input);

