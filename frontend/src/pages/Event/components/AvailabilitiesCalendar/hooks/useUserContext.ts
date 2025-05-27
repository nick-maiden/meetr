import React from "react";

const useUserContext = () => {
  const [userId, setUserId] = React.useState<string | null>(null);
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

