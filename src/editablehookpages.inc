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

procedure HOOKABLE_PAGE_CLASS.CreateHook(AClass: TEditablePageHookClass);
begin
  FreeAndNil(FHook);
  FHook := AClass.Create(Self);
end;

procedure HOOKABLE_PAGE_CLASS.AddFeatures;
begin
  FHook.AddFeaturesPre;
  inherited AddFeatures;
  FHook.AddFeaturesPost;
end;

function HOOKABLE_PAGE_CLASS.CreateNavBar: TNavBar;
begin
  Result := FHook.CreateNavBar;
end;

function HOOKABLE_PAGE_CLASS.Manager: TEditableManager;
begin
  Result := FHook.Manager;
end;

function HOOKABLE_PAGE_CLASS.HookClass: TEditablePageHookClass;
begin
  Result := TEditablePageHook;
end;

constructor HOOKABLE_PAGE_CLASS.Create;
begin
  CreateHook(HookClass);
  inherited Create;
end;

destructor HOOKABLE_PAGE_CLASS.Destroy;
begin
  FreeAndNil(FHook);
  inherited Destroy;
end;

