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
package org.apache.myfaces.extensions.validator.core.factory;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.application.FacesMessage;

/**
 * Structure for a factory that creates FacesMessages that also implement the LabeledMessage interface. The LabelMessage
 * interface adds label capabilities to the FacesMessage.
 *
 * @since 1.x.2
 */
@UsageInformation(UsageCategory.API)
public interface FacesMessageFactory
{
    /**
     * Converts the facesMessage so that is implements the LabelMessage interface. If the parameter implements already
     * the correct interface, it is returned without change.
     *
     * @param facesMessage The facesMessage to convert
     * @return A FacesMessage instance that also implements LabelMessage.
     */
    FacesMessage convert(FacesMessage facesMessage);

    /**
     * Create a LabelMessage implementing FacesMessage using the parameters as content.
     *
     * @param severity Severity for the FacesMessage.
     * @param summary summary text for the message.
     * @param detail detail test for the message.
     * @return A FacesMessage instance that also implements LabelMessage.
     */
    FacesMessage create(FacesMessage.Severity severity, String summary, String detail);
}
