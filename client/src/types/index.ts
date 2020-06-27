export interface Buffer {
  id:   number;
  hash: string;
  text: string;
}

export interface DivBuffer { 
  div: string;
  buf: Buffer;
}

export interface Student { 
  tag: "Student";
  info: User;
  grpBuffer: Buffer;
}

export interface Instructor {
  tag: "Instructor";
  info: User;
  allBuffers: Array<Buffer>;
} 

export interface None {
  tag: "None";
}

export type UserData = Student | Instructor | None;

export interface AuthInfo {
  emailAddress: string;
  password: string;
}

export interface User {
  firstName: string;
  lastName: string;
  group: string;
}

export interface LoginResponse {
  accessToken: string;
  user: UserData;
}