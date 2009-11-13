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
package org.apache.myfaces.extensions.validator.trinidad.validation.message;

import org.apache.myfaces.trinidad.util.LabeledFacesMessage;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.validation.message.LabeledMessage;

import javax.faces.context.FacesContext;

/**
 * @author Gerhard Petracek
 * @since 1.x.2
 */
@UsageInformation(UsageCategory.INTERNAL)
class TrinidadViolationMessage extends LabeledFacesMessage implements LabeledMessage
{
    private static final long serialVersionUID = 6356800689961505154L;

    public TrinidadViolationMessage(Severity severity, String summary, String detail)
    {
        super(severity, summary, detail);
    }

    public String getLabelText()
    {
        return super.getLabelAsString(FacesContext.getCurrentInstance());
    }

    public void setLabelText(String label)
    {
        super.setLabel(label);
    }
}
