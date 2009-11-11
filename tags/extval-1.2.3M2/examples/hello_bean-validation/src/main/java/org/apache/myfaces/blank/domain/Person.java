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

import org.apache.myfaces.blank.validation.group.Admin;
import org.apache.myfaces.blank.validation.group.User;
import org.apache.myfaces.blank.validation.group.Address;
import org.apache.myfaces.extensions.validator.beanval.payload.ViolationSeverity;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

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

    @NotNull(groups = Address.class, message = "street is required")
    private String street;
    @NotNull(groups = Address.class, message = "zip is required")
    private String zip;
    @NotNull(groups = Address.class, message = "city is required")
    private String city;

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

    public String getStreet()
    {
        return street;
    }

    public void setStreet(String street)
    {
        this.street = street;
    }

    public String getZip()
    {
        return zip;
    }

    public void setZip(String zip)
    {
        this.zip = zip;
    }

    public String getCity()
    {
        return city;
    }

    public void setCity(String city)
    {
        this.city = city;
    }
}
