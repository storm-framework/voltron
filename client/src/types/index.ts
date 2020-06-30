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
  class: string;
  grpBuffer: Buffer;
}

export interface Instructor {
  tag: "Instructor";
  class: string;
  allBuffers: Array<Buffer>;
} 

export interface UserData {
  user: User;
  classes: Array<ClassData>;
}

export type ClassData = Instructor | Student;

export interface AuthInfo {
  emailAddress: string;
  password: string;
}

export interface User {
  firstName: string;
  lastName: string;
}

export interface LoginResponse {
  accessToken: string;
  user: UserData;
}