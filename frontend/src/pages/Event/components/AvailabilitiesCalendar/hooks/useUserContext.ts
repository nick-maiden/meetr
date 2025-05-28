import React from "react";
import { UserId } from "global/types";

const useUserContext = () => {
  const [userId, setUserId] = React.useState<UserId | null>(null);
  const [userName, setUserName] = React.useState<string>("");
  const [isSaving, setIsSaving] = React.useState(false);

  return {
    userName,
    setUserName,
    userId,
    setUserId,
    isSaving,
    setIsSaving
  };
};

export default useUserContext;

