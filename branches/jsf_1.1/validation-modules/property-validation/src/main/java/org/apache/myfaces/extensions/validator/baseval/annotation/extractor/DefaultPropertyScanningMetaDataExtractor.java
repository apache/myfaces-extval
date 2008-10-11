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

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.SourceInformation;
import org.apache.myfaces.extensions.validator.core.metadata.DefaultSourceInformation;
import org.apache.myfaces.extensions.validator.core.metadata.PropertySourceInformationKeys;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.context.FacesContext;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Gerhard Petracek
 */
public class DefaultPropertyScanningMetaDataExtractor extends DefaultComponentMetaDataExtractor
{

    @Override
    @ToDo(Priority.MEDIUM)
    public SourceInformation extract(FacesContext facesContext, Object object)
    {
        SourceInformation sourceInformation = new DefaultSourceInformation();
        //should never occur
        if (!(object instanceof String))
        {
            return sourceInformation;
        }

        ValueBindingExpression valueBindingExpression = new ValueBindingExpression(((String) object).trim());

        List<MetaDataEntry> metaDataEntries = new ArrayList<MetaDataEntry>();

        Class entityClass = ExtValUtils.getELHelper()
            .getTypeOfValueBindingForExpression(facesContext, valueBindingExpression.getBaseExpression());

        //TODO complex components
        sourceInformation.setProperty(
            PropertySourceInformationKeys.VALUE_BINDING_EXPRESSION, valueBindingExpression.getExpressionString());

        /*
         * find and add annotations
         */
        if (entityClass != null)
        {
            //TODO map syntax support
            addPropertyAccessAnnotations(entityClass, valueBindingExpression.getProperty(), sourceInformation);
            addFieldAccessAnnotations(entityClass, valueBindingExpression.getProperty(), sourceInformation);
        }

        return sourceInformation;
    }
}
