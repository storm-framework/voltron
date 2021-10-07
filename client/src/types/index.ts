export interface Buffer {
  id:   number;
  hash: string;
  text: string;
  div:  string;
}

export interface Student {
  tag: "Student";
  class: string;
  language: string;
  grpBuffer: Buffer;
  allGroups: Array<number>;
}

export interface Instructor {
  tag: "Instructor";
  class: string;
  language: string;
  allBuffers: Array<Buffer>;
}

export interface UserData {
  user: User;
  theme: string;
  keyBinds: string;
  classes: Array<ClassData>;
}

export type ClassData = Instructor | Student;

export interface ResetInfo {
  emailAddress: string;
}

export interface ResetPassInfo {
  email: string;
  password: string;
  code: string;
}

export interface AuthInfo {
  emailAddress: string;
  password: string;
}

export interface ClassLangInfo {
  class: string;
  language: string;
}

export interface User {
  firstName: string;
  lastName: string;
  email: string;
}

export interface ClassView<T> {
  name: string;
  index: number;
  data: T;
}

export interface EnrollStudent {
  firstName: string;
  lastName: string;
  email: string;
  group: number;
}

export interface SetGroup {
  student: string; // email
  class: string;
  group: number;
}

export interface Roster {
  class: string;
  buffers?: Buffer[];
  students?: EnrollStudent[]
}