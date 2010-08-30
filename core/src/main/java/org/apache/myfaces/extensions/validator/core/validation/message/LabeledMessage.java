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
package org.apache.myfaces.extensions.validator.core.validation.message;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * Allows the setting and the retrieval of the label text that goes with an input component. This label text is used
 * for the violation message to indicate where the problem occurred.
 *
 * don't remove *Text - it would lead to an overlap with trinidad.
 * Used to create a special FacesMessage {@link ViolationMessage} that can hold the label text.
 * 
 * @author Gerhard Petracek
 * @since 1.x.2
 */
@UsageInformation(UsageCategory.API)
public interface LabeledMessage
{
    /**
     * Get the text for of the label of the input component.
     * @return label text
     */
    String getLabelText();

    /**
     * Set the text for the label of the input component.
     * @param label label text.
     */
    void setLabelText(String label);
}
