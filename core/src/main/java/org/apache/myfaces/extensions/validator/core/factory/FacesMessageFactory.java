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
 * A factory which is able to create FacesMessages that also implement the
 * {@link org.apache.myfaces.extensions.validator.core.validation.message.LabeledMessage} interface
 * (or to convert a simple {@link javax.faces.application.FacesMessage} to a
 * {@link javax.faces.application.FacesMessage} which is aware of a label.
 *
 * @since 1.x.2
 */
@UsageInformation(UsageCategory.API)
public interface FacesMessageFactory
{
    /**
     * Converts a given {@link javax.faces.application.FacesMessage} to a message which also
     * implements the {@link org.apache.myfaces.extensions.validator.core.validation.message.LabeledMessage} interface.
     * If the provided instance implements already the correct interface, it is returned without change.
     *
     * @param facesMessage The facesMessage to convert
     * @return A {@link javax.faces.application.FacesMessage} which also implements
     * {@link org.apache.myfaces.extensions.validator.core.validation.message.LabeledMessage}.
     */
    FacesMessage convert(FacesMessage facesMessage);

    /**
     * Creates a {@link javax.faces.application.FacesMessage} which also implements
     * {@link org.apache.myfaces.extensions.validator.core.validation.message.LabeledMessage}
     *
     * @param severity Severity for the message.
     * @param summary summary text for the message.
     * @param detail detail text for the message.
     * @return {@link javax.faces.application.FacesMessage} which also implements
     * {@link org.apache.myfaces.extensions.validator.core.validation.message.LabeledMessage}
     */
    FacesMessage create(FacesMessage.Severity severity, String summary, String detail);
}
