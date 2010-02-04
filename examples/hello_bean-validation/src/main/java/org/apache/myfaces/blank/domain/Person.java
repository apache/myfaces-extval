/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.blank.domain;

import org.apache.myfaces.blank.validation.NameConstraint;
import org.apache.myfaces.blank.validation.group.Admin;
import org.apache.myfaces.blank.validation.group.User;
import org.apache.myfaces.blank.validation.group.Name;
import org.apache.myfaces.extensions.validator.beanval.payload.ViolationSeverity;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@NameConstraint(groups = Name.class)
public class Person
{
    @NotNull(payload = ViolationSeverity.Warn.class)
    private String firstName;

    //demo for bean validation based validation support
    @NotNull
    @Size.List({
            @Size(min = 3, max = 6, groups = User.class),
            @Size(min = 3, max = 12, groups = Admin.class)
    })
    private String lastName;

    public String getFirstName()
    {
        return firstName;
    }

    public void setFirstName(String firstName)
    {
        this.firstName = firstName;
    }

    public String getLastName()
    {
        return lastName;
    }

    public void setLastName(String lastName)
    {
        this.lastName = lastName;
    }
}
