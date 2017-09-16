{
  This file is part of Tester Web

  Copyright (C) 2017 Alexander Kernozhitsky <sh200105@mail.ru>

  This program is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit webstrconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  SNoIndentMarker = 'No indent marker!';
  SInvalidIndentMarker = 'Invalid indent marker!';
  SConvertError = 'Could not convert %s to %s.';
  SUnclosedVariable = 'Unclosed variable specifier.';
  SConflictingModifiers = 'Conflicting or repeating modifiers at variable "%s".';
  SInvalidVariableName = 'Invalid variable name "%s".';
  SVariableNotFound = 'Variable "%s" was not found.';
  SCryptoParamsNotice = 'WARNING: Changing these parameters will lead to loss ' +
    'of ALL passwords, because encryption algorithm will change if you change ' +
    'the parameters.';
  SOwnerParamsNotice = 'WARNING: do not store your actual password here, use ' +
    'Change password option right after your first login! This settings must ' +
    'be set only for INITIAL owner profile setup. It is also good to change ' +
    'this on something random after first login.';
  SErrSessionTerminated = 'No web session active: Session was terminated';
  SErrNoSession = 'No web session active: Session was not started';
  SUnescapeExpectedChar = 'Expected character after "\"';
  SUnescapeUnknownSequence = 'Unknown escape sequence : "%s"';
  SUnescapeExpectedHexDigits = 'Expected two hex digits after "\x"';
  SUnterminatedAssignment = 'Unterminated assignment.';
  SAssignmentQuoteExpected = 'Expected "''" after "=" in assignment.';
  SUnterminatedQuotes = 'Unterminated quotes in assignment.';
  SLoopDependencies = 'Loop dependenices detected.';
  SLoopVariables = 'Variables require each other in loop.';
  SCopyright = 'Copyright &copy; 2017 Alexander Kernozhitsky';
  SLicenseNotice = 'Tester Web is licensed under the terms of <a href="~licenseLink;">GNU GPL</a> version 2 or any later version.';
  SSourcesNotice = 'Sources are available on <a href="~githubLink;">Github</a>.';
  SNoSuchUserRole = 'No such TUserRole!';
  SUsernameLength = 'Username length must be from %d to %d characters.';
  SPasswordLength = 'Password length must be from %d to %d characters.';
  SUsernameChars = 'Username must contain only the following characters: %s';
  SNameLength = 'First or last name length must be from %d to %d characters.';
  SInvalidUsernamePassword = 'Invalid username or password.';
  SInvalidPassword = 'Invalid password.';
  SUnableLogIn = 'Unable to log in (maybe, the user is blocked?)';
  SUserExists = 'User with name "%s" already exists.';
  SCannotTerminateServer = 'Cannot terminate server!';
  SAccessDenied = 'Access denied :|';
  SUpdateAlreadyUpdating = 'Already updating!';
  SUpdateNoUpdation = 'Updating is disabled! Use BeginUpdate to enable updating.';
  SUserDoesNotExist = 'User with name "%s" doesn''t exist!';
  SAuthDataNotStored = 'Authentification data is not stored!';
  SPasswordsNotEqual = 'Wrong password confirmation!';
  SAuthRequired = 'This action requires authentification.';
  SCreationPublic = '%s creation via public constructor is forbidden!';
  SLoggedAsGuest = 'You are a guest.';
  SLoggedAsUser = 'You are logged in as';
  SUserDoLogIn = 'Log in';
  SUserDoLogOut = 'Log out';
  SUserDoViewProfile = 'View profile';
  SUserDoRegister = 'Register';
  SAuthUsername = 'Username:';
  SAuthUsernamePrompt = 'Enter username';
  SAuthEnterPassword = 'Password:';
  SAuthEnterPasswordPrompt = 'Enter password';
  SAuthRetypePassword = 'Confirm password:';
  SAuthRetypePasswordPrompt = 'Enter password again';
  SAuthFirstName = 'First name:';
  SAuthFirstNamePrompt = 'Enter first name';
  SAuthLastName = 'Last name:';
  SAuthLastNamePrompt = 'Enter last name';
  SLoginTitle = 'Log in';
  SRegisterTitle = 'Register';
  SConfirmPasswordTitle = 'Password confirmation';
  SLoginRequest = 'To log into the system, please fill in the following form.';
  SLoginSubmit = 'Log in';
  SRegisterRequest = 'To register in the system, please fill in the following form.';
  SRegisterSubmit = 'Register';
  SConfirmPasswordRequest = 'Please confirm your password to continue.';
  SConfirmPasswordSubmit = 'Confirm';
  SBlockedUserRole = 'Blocked User';
  SSimpleUserRole = 'User';
  SAdminUserRole = 'Administrator';
  SOwnerUserRole = 'Server Owner';
  SMustNonNil = '%s expected to be non-nil :(';
  SProfileUserNameKey = 'Username:';
  SProfileRealNameKey = 'Real name:';
  SProfileUserRoleKey = 'User role:';
  SProfileOf = 'Profile of %s';
  SProfileRegisterTimeKey = 'Registered at:';
  SProfileLoginTimeLey = 'Last visit at:';
  SPreferredDateTimeFormat = 'dd.mm.yyyy hh:nn:ss';
  SProfileChangeRoleKey = 'Change role:';
  SProfileDoChangeRole = 'Change';
  SUsernameEmpty = 'Username must be non-empty!';
  SPasswordEmpty = 'Password must be non-empty!';
  SSettingsUpdateData = 'Update data:';
  SSettingsChangePassword = 'Change password:';
  SSettingsUpdate = 'Update';
  SSettingsUpdateSuccessful = 'Update successful.';
  SUpdateSettingsTitle = 'Update settings';
  SSettingsFirstNamePrompt = 'Enter first name to change';
  SSettingsLastNamePrompt = 'Enter last name to change';
  SSettingsOldPasswordPrompt = 'Enter old password to change';
  SSettingsNewPasswordPrompt = 'Enter new password';
  SSettingsConfirmPasswordPrompt = 'Confirm new password';
  SSettingsOldPassword = 'Old password:';
  SSettingsNewPassword = 'New password:';
  SSettingsConfirmPassword = 'Confirm password:';
  SBackToProfile = 'Back to profile';
  SSettingsNothingToUpdate = 'Nothing to update!';

implementation

end.

