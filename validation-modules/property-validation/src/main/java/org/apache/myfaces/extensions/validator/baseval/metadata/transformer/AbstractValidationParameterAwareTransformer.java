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
package org.apache.myfaces.extensions.validator.baseval.metadata.transformer;

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.application.FacesMessage;
import java.lang.annotation.Annotation;
import java.util.List;
import java.util.Map;
import java.util.Collections;

/**
 * @since x.x.3
 */
@UsageInformation(UsageCategory.REUSE)
public abstract class AbstractValidationParameterAwareTransformer implements MetaDataTransformer
{
    public Map<String, Object> convertMetaData(MetaDataEntry metaData)
    {
        if (isBlockingMetaData(metaData))
        {
            return transformMetaData(metaData);
        }
        return Collections.emptyMap();
    }

    protected abstract Map<String, Object> transformMetaData(MetaDataEntry metaData);

    protected boolean isBlockingMetaData(MetaDataEntry metaDataEntry)
    {
        FacesMessage testMessage = new FacesMessage();
        testMessage.setSeverity(FacesMessage.SEVERITY_ERROR);

        FacesMessage.Severity severity = tryToTransformViolationSeverity(metaDataEntry);

        if(severity != null)
        {
            testMessage.setSeverity(severity);
        }

        return ExtValUtils.severityBlocksSubmitForComponentId(null, testMessage);
    }

    private FacesMessage.Severity tryToTransformViolationSeverity(MetaDataEntry metaDataEntry)
    {
        List<FacesMessage.Severity> result = ExtValUtils.getValidationParameterExtractor()
                .extract(metaDataEntry.getValue(Annotation.class),
                        ExtValUtils.getValidationParameterClassFor(ViolationSeverity.class),
                        FacesMessage.Severity.class);

        if (result != null && !result.isEmpty())
        {
            return result.iterator().next();
        }
        return null;
    }
}
