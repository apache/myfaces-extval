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

import javax.faces.application.FacesMessage;

/**
 * @author Gerhard Petracek
 * @since 1.x.2
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ViolationMessage extends FacesMessage implements LabeledMessage
{
    private static final long serialVersionUID = 6903958942987711231L;
    private String label;
    private boolean summaryLabelReplaced = false;
    private boolean detailLabelReplaced = false;

    public ViolationMessage(String summary, String detail)
    {
        this(SEVERITY_ERROR, summary, detail);
    }

    public ViolationMessage(Severity severity, String summary, String detail)
    {
        setSeverity(severity);
        setSummary(summary);
        setDetail(detail);
    }

    public String getLabelText()
    {
        return label;
    }

    public void setLabelText(String label)
    {
        this.label = label;
    }

    @Override
    public String getSummary()
    {
        if(label != null && !this.summaryLabelReplaced)
        {
            setSummary(getLabeledMesssage(super.getSummary(), getLabelText()));
            this.summaryLabelReplaced = true;
        }
        return super.getSummary();
    }

    @Override
    public String getDetail()
    {
        if(label != null && !this.detailLabelReplaced)
        {
            setDetail(getLabeledMesssage(super.getDetail(), getLabelText()));
            this.detailLabelReplaced = true;
        }
        return super.getDetail();
    }

    private String getLabeledMesssage(String message, String label)
    {
        for(int i = 0; i < 3; i++)
        {
            if(message != null && message.contains("{" + i + "}"))
            {
                message = message.replace("{" + i + "}", label);
            }
        }

        return message;
    }
}
