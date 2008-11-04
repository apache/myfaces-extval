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
package org.apache.myfaces.extensions.validator.baseval.annotation.extractor;

import org.apache.myfaces.extensions.validator.core.metadata.SourceInformation;
import org.apache.myfaces.extensions.validator.core.metadata.DefaultSourceInformation;
import org.apache.myfaces.extensions.validator.core.metadata.PropertySourceInformationKeys;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.el.TargetInformationEntry;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.context.FacesContext;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultPropertyScanningMetaDataExtractor extends DefaultComponentMetaDataExtractor
{
    @Override
    @ToDo(Priority.MEDIUM)
    public SourceInformation extract(FacesContext facesContext, Object object)
    {
        SourceInformation sourceInformation = new DefaultSourceInformation();

        if (!(object instanceof TargetInformationEntry))
        {
            throw new IllegalStateException(object.getClass() + " is not a " + TargetInformationEntry.class.getName());
        }

        TargetInformationEntry targetInformationEntry = (TargetInformationEntry)object;

        Class entityClass = targetInformationEntry.getBaseObject().getClass();

        //TODO test with complex components
        sourceInformation.setProperty(
            PropertySourceInformationKeys.TARGET_INFORMATION_ENTRY, targetInformationEntry);

        /*
         * find and add annotations
         */
        addPropertyAccessAnnotations(entityClass, targetInformationEntry.getProperty(), sourceInformation);
        addFieldAccessAnnotations(entityClass, targetInformationEntry.getProperty(), sourceInformation);

        return sourceInformation;
    }
}
