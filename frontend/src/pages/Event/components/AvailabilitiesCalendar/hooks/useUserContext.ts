import React from "react";
import { UserId } from "../../../../../types";

const useUserContext = () => {
  const [userId, setUserId] = React.useState<UserId | null>(null);
  const [userName, setUserName] = React.useState<string>("");
  const [isSaving, setIsSaving] = React.useState(false);
  const [hasConfirmedName, setHasConfirmedName] = React.useState(false);

  return {
    userName,
    setUserName,
    userId,
    setUserId,
    hasConfirmedName,
    setHasConfirmedName,
    isSaving,
    setIsSaving
  };
};

export default useUserContext;

